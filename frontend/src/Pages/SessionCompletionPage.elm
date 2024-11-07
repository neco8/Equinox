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
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import Route exposing (Route(..))


{-| メッセージ
-}
type Msg
    = NavigateToRoute Route


{-| ビュー
-}
view : { a | duration : Int, txt : String } -> Html Msg
view { duration, txt } =
    div [ attribute "role" "session-completion" ]
        [ text txt
        , text <| "完了" ++ String.fromInt duration ++ "秒"
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
