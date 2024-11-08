module Pages.SessionPreparationPage exposing
    ( Model, init
    , Msg(..)
    , update
    , PracticeStyle(..), view
    )

{-| このモジュールはセッション準備ページに関係するものです。


### モデル

@docs Model, init


### メッセージ

@docs Msg


### アップデート

@docs update


### ビュー

@docs PracticeStyle, view

-}

import Browser.Navigation as Nav
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (attribute, disabled)
import Html.Events exposing (onClick, onInput)
import Route exposing (Route(..))
import Types.BreathingMethod exposing (BreathingMethod)


{-| Model
-}
type alias Model =
    { sessionDurationInput : String
    , inhaleDurationInput : String
    , inhaleHoldDurationInput : String
    , exhaleDurationInput : String
    , exhaleHoldDurationInput : String
    }


{-| 初期化
-}
init : () -> Model
init _ =
    { sessionDurationInput = ""
    , inhaleDurationInput = ""
    , inhaleHoldDurationInput = ""
    , exhaleDurationInput = ""
    , exhaleHoldDurationInput = ""
    }


{-| メッセージ
-}
type Msg
    = InputSessionDuration String
    | InputInhaleDuration String
    | InputInhaleHoldDuration String
    | InputExhaleDuration String
    | InputExhaleHoldDuration String
    | PrepareSessionPageNavigateToRoute Route


{-| アップデート
-}
update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        InputSessionDuration duration ->
            ( { model | sessionDurationInput = duration }, Cmd.none )

        InputInhaleDuration duration ->
            ( { model | inhaleDurationInput = duration }, Cmd.none )

        InputInhaleHoldDuration duration ->
            ( { model | inhaleHoldDurationInput = duration }, Cmd.none )

        InputExhaleDuration duration ->
            ( { model | exhaleDurationInput = duration }, Cmd.none )

        InputExhaleHoldDuration duration ->
            ( { model | exhaleHoldDurationInput = duration }, Cmd.none )

        PrepareSessionPageNavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )


{-| 練習スタイル

手動で設定したBreathingMethodかもしくは既存のBreathingMethodかを選択する

-}
type PracticeStyle
    = Manual
    | Preset BreathingMethod


{-| ビュー
-}
view : { a | txt : String, practiceStyle : PracticeStyle, route : Int -> Route } -> Model -> Html Msg
view { txt, practiceStyle, route } { sessionDurationInput } =
    let
        breathingMethodControls =
            case practiceStyle of
                Manual ->
                    div []
                        [ input
                            [ attribute "aria-label" "inhale-duration-input"
                            , onInput InputInhaleDuration
                            ]
                            []
                        , input
                            [ attribute "aria-label" "inhale-hold-duration-input"
                            , onInput InputInhaleHoldDuration
                            ]
                            []
                        , input
                            [ attribute "aria-label" "exhale-duration-input"
                            , onInput InputExhaleDuration
                            ]
                            []
                        , input
                            [ attribute "aria-label" "exhale-hold-duration-input"
                            , onInput InputExhaleHoldDuration
                            ]
                            []
                        ]

                Preset m ->
                    div []
                        [ text <| String.fromInt m.inhaleDuration
                        , text <| String.fromInt m.inhaleHoldDuration
                        , text <| String.fromInt m.exhaleDuration
                        , text <| String.fromInt m.exhaleHoldDuration
                        ]
    in
    div [ attribute "role" "preparation" ]
        [ text txt
        , input
            [ attribute "aria-label" "session-duration"
            , onInput InputSessionDuration
            ]
            []
        , breathingMethodControls
        , button
            [ attribute "aria-label" "start-session"
            , case String.toInt sessionDurationInput of
                Just duration ->
                    onClick (PrepareSessionPageNavigateToRoute (route duration))

                Nothing ->
                    disabled True
            ]
            [ text "セッション開始" ]
        ]
