module Route exposing
    ( Route(..)
    , toString, fromUrl, href
    )

{-| ルーティングを提供します。


### ルート

@docs Route


### Helper関数

@docs toString, fromUrl, href

-}

import Html exposing (Attribute)
import Html.Attributes
import Types.BreathingMethod exposing (BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration)
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
    | PresetSessionPreparationRoute BreathingMethodId
    | ManualSessionPreparationRoute
    | PresetSessionRoute BreathingMethodId (Maybe Int)
    | ManualSessionRoute (Maybe Int) (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)
    | PresetSessionCompletionRoute BreathingMethodId (Maybe Int)
    | ManualSessionCompletionRoute (Maybe Int)
    | StatisticsRoute
    | SettingsRoute
    | SourceSelectionRoute
    | BreathingMethodEditRoute BreathingMethodId


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
        , Parser.map PresetSessionPreparationRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "preparation" </> uuidParser)
        , Parser.map ManualSessionPreparationRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "preparation")
        , Parser.map PresetSessionRoute
            (Parser.s "breathing-methods" </> Parser.s "session" </> Parser.s "running" </> uuidParser <?> Query.int "duration")
        , Parser.map ManualSessionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "running"
                <?> Query.int "duration"
                <?> Query.custom "inhale-duration"
                        (List.head
                            >> Maybe.andThen String.toInt
                            >> Maybe.andThen toInhaleDuration
                        )
                <?> Query.custom "inhale-hold-duration"
                        (List.head
                            >> Maybe.andThen String.toInt
                            >> Maybe.andThen toInhaleHoldDuration
                        )
                <?> Query.custom "exhale-duration"
                        (List.head
                            >> Maybe.andThen String.toInt
                            >> Maybe.andThen toExhaleDuration
                        )
                <?> Query.custom "exhale-hold-duration"
                        (List.head
                            >> Maybe.andThen String.toInt
                            >> Maybe.andThen toExhaleHoldDuration
                        )
            )
        , Parser.map PresetSessionCompletionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "completion"
                </> uuidParser
                <?> Query.int "finished-duration"
            )
        , Parser.map ManualSessionCompletionRoute
            (Parser.s "breathing-methods"
                </> Parser.s "session"
                </> Parser.s "completion"
                <?> Query.int "finished-duration"
            )
        , Parser.map StatisticsRoute
            (Parser.s "statistics")
        , Parser.map SettingsRoute
            (Parser.s "settings")
        , Parser.map SourceSelectionRoute
            (Parser.s "breathing-methods" </> Parser.s "source-selection")
        , Parser.map BreathingMethodEditRoute
            (Parser.s "breathing-methods" </> Parser.s "edit" </> uuidParser)
        ]


toString : Route -> String
toString route =
    case route of
        HomeRoute ->
            "/"

        PresetSessionPreparationRoute id ->
            "/breathing-methods/session/preparation/" ++ Uuid.toString id

        ManualSessionPreparationRoute ->
            "/breathing-methods/session/preparation"

        PresetSessionRoute id duration ->
            "/breathing-methods/session/running/"
                ++ Uuid.toString id
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "duration" <| Maybe.withDefault "" <| Maybe.map String.fromInt duration
                    ]

        ManualSessionRoute duration inhale inhaleHold exhale exhaleHold ->
            "/breathing-methods/session/running"
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map String.fromInt duration)
                        )
                    , Url.Builder.string "inhale-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map (String.fromInt << fromInhaleDuration) inhale)
                        )
                    , Url.Builder.string "inhale-hold-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map (String.fromInt << fromInhaleHoldDuration) inhaleHold)
                        )
                    , Url.Builder.string "exhale-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map (String.fromInt << fromExhaleDuration) exhale)
                        )
                    , Url.Builder.string "exhale-hold-duration"
                        (Maybe.withDefault
                            ""
                            (Maybe.map (String.fromInt << fromExhaleHoldDuration) exhaleHold)
                        )
                    ]

        PresetSessionCompletionRoute id duration ->
            "/breathing-methods/session/completion/"
                ++ Uuid.toString id
                ++ Url.Builder.toQuery
                    [ Url.Builder.string "finished-duration" <| Maybe.withDefault "" <| Maybe.map String.fromInt duration
                    ]

        ManualSessionCompletionRoute duration ->
            "/breathing-methods/session/completion"
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

        BreathingMethodEditRoute id ->
            "/breathing-methods/edit/" ++ Uuid.toString id


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


href : Route -> Attribute msg
href route =
    Html.Attributes.href <| toString route
