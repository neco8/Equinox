module Pages.SessionPreparationPage exposing
    ( Model, init
    , Msg
    , update
    , PracticeStyle(..), view
    )

{-|


## Session Preparation Page

このモジュールはセッション準備ページに関係するものです。


### モデル

@docs Model, init


### メッセージ

@docs Msg


### アップデート

@docs update


### ビュー

@docs PracticeStyle, view

-}

import BreathingMethodDurationInput
import Browser.Navigation as Nav
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (attribute, disabled, style)
import Html.Events exposing (onClick, onInput)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration)
import Types.Session exposing (toDuration)


{-| Model
-}
type Model
    = ModelLoading PracticeStyle
    | ModelLoaded InternalModel


{-| 内部で利用されるModel
-}
type alias InternalModel =
    { practiceStyle : ValidPracticeStyle
    , sessionDurationInput : String
    , inhaleDurationInput : String
    , inhaleHoldDurationInput : String
    , exhaleDurationInput : String
    , exhaleHoldDurationInput : String
    }


{-| 練習スタイルを検証する

既存の呼吸法のリストに存在するかを検証する

-}
validatePracticeStyle : List BreathingMethod -> PracticeStyle -> Maybe ValidPracticeStyle
validatePracticeStyle breathingMethods practiceStyle =
    case practiceStyle of
        ManualPracticeStyle ->
            Just Manual

        PresetPracticeStyle id ->
            List.Extra.find (.id >> (==) id) breathingMethods
                |> Maybe.map Preset


{-| 初期化
-}
init : RemoteData e (List BreathingMethod) -> PracticeStyle -> ( Model, Cmd Msg )
init remote practiceStyle =
    case remote of
        NotAsked ->
            ( ModelLoading practiceStyle, Cmd.none )

        Loading ->
            ( ModelLoading practiceStyle, Cmd.none )

        Failure _ ->
            ( ModelLoading practiceStyle
              -- ホーム画面へ遷移する - [ ] TODO: エラーメッセージを表示する
            , NavigateToRoute HomeRoute
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        Success breathingMethods ->
            case validatePracticeStyle breathingMethods practiceStyle of
                Just valid ->
                    ( initInternal valid
                        |> ModelLoaded
                    , Cmd.none
                    )

                Nothing ->
                    ( ModelLoading practiceStyle
                      -- 存在しない呼吸法のため、ホーム画面へ遷移する - [ ] TODO: エラーメッセージを表示する
                    , NavigateToRoute HomeRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now
                    )


{-| 内部で利用される初期化
-}
initInternal : ValidPracticeStyle -> InternalModel
initInternal m =
    { practiceStyle = m
    , sessionDurationInput = ""
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
    | NavigateToRoute Route


{-| アップデート
-}
update : RemoteData e (List BreathingMethod) -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update remote key msg model =
    case model of
        ModelLoading practiceStyle ->
            case remote of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure _ ->
                    ( model
                      -- 失敗したのでホーム画面へ遷移する
                    , NavigateToRoute HomeRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now
                    )

                Success breathingMethods ->
                    case validatePracticeStyle breathingMethods practiceStyle of
                        Just valid ->
                            ( initInternal valid
                                |> ModelLoaded
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , NavigateToRoute HomeRoute
                                |> always
                                |> Task.perform
                                |> (|>) Time.now
                            )

        ModelLoaded internalModel ->
            updateInternal key msg internalModel
                |> Tuple.mapFirst ModelLoaded


{-| 内部で利用されるアップデート
-}
updateInternal : Nav.Key -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal key msg model =
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

        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )


{-| 練習スタイル

手動で設定したBreathingMethodかもしくは既存のBreathingMethodかを選択する

-}
type PracticeStyle
    = ManualPracticeStyle
    | PresetPracticeStyle BreathingMethodId


{-| 正常な練習スタイル
-}
type ValidPracticeStyle
    = Manual
    | Preset BreathingMethod


{-| ビュー
-}
view : { a | txt : String } -> Model -> Html Msg
view { txt } model =
    case model of
        ModelLoaded loaded ->
            let
                breathingMethodControls =
                    case loaded.practiceStyle of
                        Manual ->
                            BreathingMethodDurationInput.view
                                (BreathingMethodDurationInput.Config
                                    InputInhaleDuration
                                    InputInhaleHoldDuration
                                    InputExhaleDuration
                                    InputExhaleHoldDuration
                                )
                                loaded

                        Preset m ->
                            div []
                                [ span [ attribute "aria-label" "inhale" ] [ text <| String.fromInt <| fromInhaleDuration m.inhaleDuration ]
                                , span [ attribute "aria-label" "inhale-hold" ] [ text <| String.fromInt <| fromInhaleHoldDuration m.inhaleHoldDuration ]
                                , span [ attribute "aria-label" "exhale" ] [ text <| String.fromInt <| fromExhaleDuration m.exhaleDuration ]
                                , span [ attribute "aria-label" "exhale-hold" ] [ text <| String.fromInt <| fromExhaleHoldDuration m.exhaleHoldDuration ]
                                ]

                route duration =
                    case loaded.practiceStyle of
                        Manual ->
                            ManualSessionRoute (Just duration)
                                (Maybe.andThen Types.BreathingMethod.toInhaleDuration <| String.toInt loaded.inhaleDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toInhaleHoldDuration <| String.toInt loaded.inhaleHoldDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toExhaleDuration <| String.toInt loaded.exhaleDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toExhaleHoldDuration <| String.toInt loaded.exhaleHoldDurationInput)

                        Preset method ->
                            PresetSessionRoute method.id (Just duration)
            in
            div [ attribute "role" "preparation" ]
                [ text txt
                , input
                    [ attribute "aria-label" "session-duration-input"
                    , onInput InputSessionDuration
                    ]
                    []
                , breathingMethodControls
                , button
                    [ attribute "aria-label" "start-session"
                    , case Maybe.andThen toDuration <| String.toInt loaded.sessionDurationInput of
                        Just duration ->
                            onClick (NavigateToRoute (route duration))

                        Nothing ->
                            disabled True
                    ]
                    [ text "セッション開始" ]
                , div [ attribute "aria-label" "backdrop", style "width" "10px", style "height" "10px", style "background-color" "gray" ] []
                ]

        ModelLoading _ ->
            div []
                [ text "Loading..." ]
