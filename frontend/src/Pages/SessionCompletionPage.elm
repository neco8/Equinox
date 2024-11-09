module Pages.SessionCompletionPage exposing
    ( Msg
    , view
    , update
    )

{-| このモジュールはセッション完了ページに関するものです。


### メッセージ

@docs Msg


### ビュー

@docs view


### アップデート

@docs update

-}

import Browser.Navigation as Nav
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onClick)
import Route exposing (Route(..))
import Types.Session exposing (Duration, fromDuration)


{-| メッセージ
-}
type Msg
    = NavigateToRoute Route


{-| ビュー
-}
view : { a | duration : Duration, txt : String } -> Html Msg
view { duration, txt } =
    div [ attribute "role" "session-completion" ]
        [ text txt
        , text "完了"
        , span
            [ attribute "aria-label" "finish-duration"
            , value (String.fromInt <| fromDuration duration)
            ]
            [ text <| (String.fromInt << fromDuration) duration ]
        , text "秒"
        , button
            [ attribute "aria-label" "next" ]
            [ text "次へ" ]
        , button
            [ attribute "aria-label" "finish"
            , onClick (NavigateToRoute StatisticsRoute)
            ]
            [ text "完了" ]
        ]


{-| アップデート
-}
update : Nav.Key -> Msg -> Cmd Msg
update key msg =
    case msg of
        NavigateToRoute route ->
            Nav.pushUrl key (Route.toString route)
