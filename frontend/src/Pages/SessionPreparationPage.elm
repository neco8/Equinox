module Pages.SessionPreparationPage exposing
    ( Model, init
    , Msg, noOp
    , update
    , PracticeStyle(..), view
    )

{-|


## Session Preparation Page

このモジュールはセッション準備ページに関係するものです。


### モデル

@docs Model, init


### メッセージ

@docs Msg, noOp


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
import Maybe.Extra
import Pages.SessionPage as SessionPage
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration)
import Types.Session exposing (Duration, toDuration)


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
    }


{-| 練習スタイルを検証する

既存の呼吸法のリストに存在するかを検証する

-}
validatePracticeStyle : List BreathingMethod -> PracticeStyle -> Maybe ValidPracticeStyle
validatePracticeStyle breathingMethods practiceStyle =
    case practiceStyle of
        ManualPracticeStyle ->
            Just
                (Manual
                    { inhaleDurationInput = ""
                    , inhaleHoldDurationInput = ""
                    , exhaleDurationInput = ""
                    , exhaleHoldDurationInput = ""
                    }
                )

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
    }


{-| カスタム練習のときのみ利用される入力メッセージ
-}
type ManualInputMsg
    = InputInhaleDuration String
    | InputInhaleHoldDuration String
    | InputExhaleDuration String
    | InputExhaleHoldDuration String


{-| メッセージ
-}
type Msg
    = InputSessionDuration String
    | ManualInputMsg ManualInputMsg
    | NavigateToRoute Route
    | NoOp


{-| メッセージ: NoOp
画面更新用に利用される
-}
noOp : Msg
noOp =
    NoOp


{-| カスタム練習の入力メッセージを処理する
-}
handleManualInputMsg :
    ManualInputMsg
    ->
        { inhaleDurationInput : String
        , inhaleHoldDurationInput : String
        , exhaleDurationInput : String
        , exhaleHoldDurationInput : String
        }
    ->
        { inhaleDurationInput : String
        , inhaleHoldDurationInput : String
        , exhaleDurationInput : String
        , exhaleHoldDurationInput : String
        }
handleManualInputMsg msg model =
    case msg of
        InputInhaleDuration duration ->
            { model | inhaleDurationInput = duration }

        InputInhaleHoldDuration duration ->
            { model | inhaleHoldDurationInput = duration }

        InputExhaleDuration duration ->
            { model | exhaleDurationInput = duration }

        InputExhaleHoldDuration duration ->
            { model | exhaleHoldDurationInput = duration }


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
        NoOp ->
            ( model, Cmd.none )

        InputSessionDuration duration ->
            ( { model | sessionDurationInput = duration }, Cmd.none )

        ManualInputMsg subMsg ->
            case model.practiceStyle of
                Manual manual ->
                    ( { model | practiceStyle = Manual (handleManualInputMsg subMsg manual) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
        { inhaleDurationInput : String
        , inhaleHoldDurationInput : String
        , exhaleDurationInput : String
        , exhaleHoldDurationInput : String
        }
    | Preset BreathingMethod


{-| 入力を検証する
-}
validateInput : InternalModel -> Maybe { sessionDuration : Duration, selectedBreathingMethod : SessionPage.SelectedBreathingMethod }
validateInput { sessionDurationInput, practiceStyle } =
    Just (\sessionDuration selectedBreathingMethod -> { sessionDuration = sessionDuration, selectedBreathingMethod = selectedBreathingMethod })
        |> Maybe.Extra.andMap (sessionDurationInput |> String.toInt |> Maybe.andThen toDuration)
        |> Maybe.Extra.andMap
            (case practiceStyle of
                Manual manual ->
                    Just
                        (\i ih e eh ->
                            SessionPage.CustomBreathingMethod
                                { inhaleDuration = Just i
                                , inhaleHoldDuration = Just ih
                                , exhaleDuration = Just e
                                , exhaleHoldDuration = Just eh
                                }
                        )
                        |> Maybe.Extra.andMap (manual.inhaleDurationInput |> String.toInt |> Maybe.andThen toInhaleDuration)
                        |> Maybe.Extra.andMap (manual.inhaleHoldDurationInput |> String.toInt |> Maybe.andThen toInhaleHoldDuration)
                        |> Maybe.Extra.andMap (manual.exhaleDurationInput |> String.toInt |> Maybe.andThen toExhaleDuration)
                        |> Maybe.Extra.andMap (manual.exhaleHoldDurationInput |> String.toInt |> Maybe.andThen toExhaleHoldDuration)

                Preset method ->
                    Just (SessionPage.PresetBreathingMethod method.id)
            )


{-| ビュー
-}
view : { a | txt : String } -> Model -> Html Msg
view { txt } model =
    case model of
        ModelLoaded loaded ->
            let
                breathingMethodControls =
                    Html.map ManualInputMsg <|
                        case loaded.practiceStyle of
                            Manual manual ->
                                BreathingMethodDurationInput.view
                                    (BreathingMethodDurationInput.Config
                                        InputInhaleDuration
                                        InputInhaleHoldDuration
                                        InputExhaleDuration
                                        InputExhaleHoldDuration
                                    )
                                    manual

                            Preset m ->
                                div []
                                    [ span [ attribute "aria-label" "inhale" ] [ text <| String.fromInt <| fromInhaleDuration m.inhaleDuration ]
                                    , span [ attribute "aria-label" "inhale-hold" ] [ text <| String.fromInt <| fromInhaleHoldDuration m.inhaleHoldDuration ]
                                    , span [ attribute "aria-label" "exhale" ] [ text <| String.fromInt <| fromExhaleDuration m.exhaleDuration ]
                                    , span [ attribute "aria-label" "exhale-hold" ] [ text <| String.fromInt <| fromExhaleHoldDuration m.exhaleHoldDuration ]
                                    ]

                route duration =
                    case loaded.practiceStyle of
                        Manual manual ->
                            ManualSessionRoute (Just duration)
                                (Maybe.andThen Types.BreathingMethod.toInhaleDuration <| String.toInt manual.inhaleDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toInhaleHoldDuration <| String.toInt manual.inhaleHoldDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toExhaleDuration <| String.toInt manual.exhaleDurationInput)
                                (Maybe.andThen Types.BreathingMethod.toExhaleHoldDuration <| String.toInt manual.exhaleHoldDurationInput)

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
                    ([ attribute "aria-label" "start-session"
                     , case Maybe.andThen toDuration <| String.toInt loaded.sessionDurationInput of
                        Just duration ->
                            onClick (NavigateToRoute (route duration))

                        Nothing ->
                            disabled True
                     ]
                        ++ (validateInput loaded
                                |> (\validated ->
                                        List.filterMap ((|>) validated)
                                            [ Maybe.Extra.isNothing
                                                >> disabled
                                                >> Just
                                            , Maybe.map
                                                (.sessionDuration
                                                    >> route
                                                    >> NavigateToRoute
                                                    >> onClick
                                                )
                                            ]
                                   )
                           )
                    )
                    [ text "セッション開始" ]
                , div [ attribute "aria-label" "backdrop", style "width" "10px", style "height" "10px", style "background-color" "gray" ] []
                ]

        ModelLoading _ ->
            div []
                [ text "Loading..." ]
