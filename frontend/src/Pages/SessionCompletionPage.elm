module Pages.SessionCompletionPage exposing
    ( Msg
    , view
    , update
    , Model, init
    )

{-|


## Session Completion Page

このモジュールはセッション完了ページに関するものです。


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
import Pages.SessionPreparationPage exposing (PracticeStyle(..))
import Route exposing (Route(..))
import Task
import Time
import Types.Session exposing (Duration, fromDuration)
import Uuid


{-| メッセージ
-}
type Msg
    = NavigateToRoute Route


{-| Model
-}
type Model
    = ModelInvalid PracticeStyle
    | ModelValid
        { duration : Duration
        , practiceStyle : PracticeStyle
        }


{-| 初期化
-}
init : Maybe Duration -> PracticeStyle -> ( Model, Cmd Msg )
init mduration practiceStyle =
    case mduration of
        Just duration ->
            ( ModelValid
                { duration = duration
                , practiceStyle = practiceStyle
                }
            , Cmd.none
            )

        Nothing ->
            ( ModelInvalid practiceStyle
            , (case practiceStyle of
                PresetPracticeStyle id ->
                    PresetSessionPreparationRoute id

                ManualPracticeStyle ->
                    ManualSessionPreparationRoute
              )
                |> NavigateToRoute
                |> always
                |> Task.perform
                |> (|>) Time.now
            )


{-| ビュー
-}
view : Model -> Html Msg
view model =
    case model of
        ModelValid { duration, practiceStyle } ->
            let
                txt =
                    case practiceStyle of
                        PresetPracticeStyle id ->
                            "完了画面 - ID: " ++ Uuid.toString id

                        ManualPracticeStyle ->
                            "カスタム完了画面"
            in
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

        ModelInvalid _ ->
            text "durationが存在していません。"


{-| アップデート
-}
update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )
