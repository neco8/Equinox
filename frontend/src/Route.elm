module Route exposing
    ( Route(..)
    , toString, fromUrl, href
    )

{-| ルーティングを提供します。


### ルート

@docs Route


### Helper関数

@docs toString, fromUrl, href


### TODO

  - [ ] TODO: ルーティングの名前が適当なので、e2eの名前も含めて変更する

-}

import Html exposing (Attribute)
import Html.Attributes
import Types.BreathingMethod exposing (BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration)
import Url
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)


{-| ルート

RouteというSuffixをつけることで、Pageとの区別を明確にしている。

-}
type Route
    = HomeRoute
    | PrepareSessionRoute BreathingMethodId
    | PrepareCustomSessionRoute
    | StartSessionRoute BreathingMethodId (Maybe Int)
    | StartCustomSessionRoute (Maybe Int) (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)
    | CompleteSessionRoute BreathingMethodId (Maybe Int)
    | CompleteCustomSessionRoute (Maybe Int)
    | StatisticsRoute
    | SettingsRoute
    | SourceSelectionRoute
    | EditBreathingMethodRoute BreathingMethodId


{-| UUIDのURLパーサー
-}
uuidParser : Parser (Uuid -> a) a
uuidParser =
    Parser.custom "UUID" Uuid.fromString


{-| ルートのパーサー
ルートに変更があった際は、ここにも変更を反映すること。
-}
parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map PrepareSessionRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "prepare" </> uuidParser)
        , Parser.map PrepareCustomSessionRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "prepare")
        , Parser.map StartSessionRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "start" </> uuidParser <?> Query.int "duration")
        , Parser.map StartCustomSessionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "start"
                <?> Query.int "duration"
                <?> Query.int "inhale-duration"
                <?> Query.int "inhale-hold-duration"
                <?> Query.int "exhale-duration"
                <?> Query.int "exhale-hold-duration"
            )
        , Parser.map CompleteSessionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "complete"
                </> uuidParser
                <?> Query.int "finished-duration"
            )
        , Parser.map CompleteCustomSessionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "complete"
                <?> Query.int "finished-duration"
            )
        , Parser.map StatisticsRoute
            (Parser.s "statistics")
        , Parser.map SettingsRoute
            (Parser.s "settings")
        , Parser.map SourceSelectionRoute
            (Parser.s "breathing-methods" </> Parser.s "source-selection")
        , Parser.map EditBreathingMethodRoute
            (Parser.s "breathing-methods" </> Parser.s "edit" </> uuidParser)
        ]


toString : Route -> String
toString route =
    case route of
        HomeRoute ->
            "/"

        PrepareSessionRoute id ->
            "/breathing-methods/session/prepare/" ++ Uuid.toString id

        PrepareCustomSessionRoute ->
            "/breathing-methods/session/prepare"

        StartSessionRoute id duration ->
            "/breathing-methods/session/start/"
                ++ Uuid.toString id
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "duration" <| Maybe.withDefault "" <| Maybe.map String.fromInt duration
                    ]

        StartCustomSessionRoute duration inhale inhaleHold exhale exhaleHold ->
            "/breathing-methods/session/start"
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt duration)
                        )
                    , Url.Builder.string "inhale-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt inhale)
                        )
                    , Url.Builder.string "inhale-hold-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt inhaleHold)
                        )
                    , Url.Builder.string "exhale-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt exhale)
                        )
                    , Url.Builder.string "exhale-hold-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt exhaleHold)
                        )
                    ]

        CompleteSessionRoute id duration ->
            "/breathing-methods/session/complete/"
                ++ Uuid.toString id
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "finished-duration" <| Maybe.withDefault "" <| Maybe.map String.fromInt duration
                    ]

        CompleteCustomSessionRoute duration ->
            "/breathing-methods/session/complete"
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "finished-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt duration)
                        )
                    ]

        StatisticsRoute ->
            "/statistics"

        SettingsRoute ->
            "/settings"

        SourceSelectionRoute ->
            "/breathing-methods/source-selection"

        EditBreathingMethodRoute id ->
            "/breathing-methods/edit/" ++ Uuid.toString id


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


href : Route -> Attribute msg
href route =
    Html.Attributes.href <| toString route
