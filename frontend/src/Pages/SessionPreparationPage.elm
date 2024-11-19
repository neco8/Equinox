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
import Html exposing (button, div, h1, input, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Icon
import List.Extra
import Maybe.Extra
import Nav
import Pages.SessionPage as SessionPage
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, PhaseType(..), fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration)
import Types.Session exposing (Duration, toDuration)
import View exposing (View)


{-| Model

    type Model
        = ModelLoading PracticeStyle
        | ModelLoaded InternalModel

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


{-| validatePracticeStyleの結果

未知という状態が存在する

    type ValidatePracticeStyleResult
        = Valid ValidPracticeStyle
        | Invalid
        | NotYetKnown

-}
type ValidatePracticeStyleResult
    = Valid ValidPracticeStyle
    | Invalid
    | NotYetKnown


{-| 練習スタイルを検証する

既存の呼吸法のリストに存在するかを検証する

-}
validatePracticeStyle : RemoteData e (List BreathingMethod) -> PracticeStyle -> ValidatePracticeStyleResult
validatePracticeStyle remote practiceStyle =
    case practiceStyle of
        ManualPracticeStyle ->
            Valid
                (Manual
                    { inhaleDurationInput = ""
                    , inhaleHoldDurationInput = ""
                    , exhaleDurationInput = ""
                    , exhaleHoldDurationInput = ""
                    }
                )

        PresetPracticeStyle id ->
            case remote of
                Success breathingMethods ->
                    case
                        List.Extra.find (.id >> (==) id) breathingMethods
                            |> Maybe.map Preset
                    of
                        Just valid ->
                            Valid valid

                        Nothing ->
                            Invalid

                Failure _ ->
                    -- リモートデータが失敗しているため、idから検証はできない。失敗とする
                    Invalid

                NotAsked ->
                    -- リモートデータがまだ取得されていないため、idから検証はできない。未知とする
                    NotYetKnown

                Loading ->
                    -- リモートデータが取得中のため、idから検証はできない。未知とする
                    NotYetKnown


{-| 初期化
-}
init : RemoteData e (List BreathingMethod) -> PracticeStyle -> ( Model, Cmd Msg )
init remote practiceStyle =
    case validatePracticeStyle remote practiceStyle of
        Valid valid ->
            ( initInternal valid
                |> ModelLoaded
            , Cmd.none
            )

        Invalid ->
            ( ModelLoading practiceStyle
              -- 存在しない呼吸法のため、ホーム画面へ遷移する - [ ] TODO: エラーメッセージを表示する
            , NavigateToRoute HomeRoute
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        NotYetKnown ->
            ( ModelLoading practiceStyle, Cmd.none )


{-| 内部で利用される初期化
-}
initInternal : ValidPracticeStyle -> InternalModel
initInternal m =
    { practiceStyle = m
    , sessionDurationInput = ""
    }


{-| カスタム練習のときのみ利用される入力メッセージ

    type ManualInputMsg
        = InputInhaleDuration String
        | InputInhaleHoldDuration String
        | InputExhaleDuration String
        | InputExhaleHoldDuration String

-}
type ManualInputMsg
    = InputInhaleDuration String
    | InputInhaleHoldDuration String
    | InputExhaleDuration String
    | InputExhaleHoldDuration String


{-| メッセージ

    type Msg
        = InputSessionDuration String
        | ManualInputMsg ManualInputMsg
        | NavigateToRoute Route
        | NoOp

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
            case validatePracticeStyle remote practiceStyle of
                Valid valid ->
                    updateInternal key msg (initInternal valid)
                        |> Tuple.mapFirst ModelLoaded

                Invalid ->
                    ( model
                    , NavigateToRoute HomeRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now
                    )

                NotYetKnown ->
                    ( model, Cmd.none )

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

    type PracticeStyle
        = ManualPracticeStyle
        | PresetPracticeStyle BreathingMethodId

-}
type PracticeStyle
    = ManualPracticeStyle
    | PresetPracticeStyle BreathingMethodId


{-| 正常な練習スタイル

    type ValidPracticeStyle
        = Manual
            { inhaleDurationInput : String
            , inhaleHoldDurationInput : String
            , exhaleDurationInput : String
            , exhaleHoldDurationInput : String
            }
        | Preset BreathingMethod

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
        |> Maybe.Extra.andMap (sessionDurationInput |> String.toInt |> Maybe.map ((*) 60) |> Maybe.andThen toDuration)
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
view : Model -> View Msg
view model =
    { nav =
        Nav.initialConfig
            |> Nav.withSettings (NavigateToRoute SettingsRoute) (Just 30)
            |> Just
    , footer = True
    , view =
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
                                    div [ class "grid grid-cols-2 gap-4 mb-8" ] <|
                                        List.map
                                            (\phaseType ->
                                                div
                                                    [ class "flex flex-col items-center p-4 rounded-lg"
                                                    , (toColorClass phaseType).bgColorClass
                                                    ]
                                                    [ div [ class "flex items-center space-x-2 mb-2" ]
                                                        [ toIcon phaseType
                                                        , span [ class "text-sm text-gray-600" ] [ text <| verbosePhaseType phaseType ]
                                                        ]
                                                    , span
                                                        [ toAriaLabel phaseType
                                                        , class "text-2xl font-semibold"
                                                        , (toColorClass phaseType).textColorClass
                                                        ]
                                                        [ text <| String.fromInt (toDuration phaseType m) ]
                                                    , span [ class "text-xs text-gray-500 mt-1" ] [ text "秒" ]
                                                    ]
                                            )
                                            [ Inhale, InhaleHold, Exhale, ExhaleHold ]

                    verbosePhaseType phaseType =
                        case phaseType of
                            Inhale ->
                                "吸う"

                            InhaleHold ->
                                "止める"

                            Exhale ->
                                "吐く"

                            ExhaleHold ->
                                "止める"

                    toDuration phaseType m =
                        case phaseType of
                            Inhale ->
                                fromInhaleDuration m.inhaleDuration

                            InhaleHold ->
                                fromInhaleHoldDuration m.inhaleHoldDuration

                            Exhale ->
                                fromExhaleDuration m.exhaleDuration

                            ExhaleHold ->
                                fromExhaleHoldDuration m.exhaleHoldDuration

                    toColorClass phaseType =
                        case phaseType of
                            Inhale ->
                                { textColorClass = class "text-blue-600"
                                , bgColorClass = class "bg-blue-50"
                                }

                            InhaleHold ->
                                { textColorClass = class "text-indigo-600"
                                , bgColorClass = class "bg-indigo-50"
                                }

                            Exhale ->
                                { textColorClass = class "text-purple-600"
                                , bgColorClass = class "bg-purple-50"
                                }

                            ExhaleHold ->
                                { textColorClass = class "text-pink-600"
                                , bgColorClass = class "bg-pink-50"
                                }

                    phaseTypeToString phaseType =
                        case phaseType of
                            Inhale ->
                                "inhale"

                            InhaleHold ->
                                "inhale-hold"

                            Exhale ->
                                "exhale"

                            ExhaleHold ->
                                "exhale-hold"

                    toIcon phaseType =
                        case phaseType of
                            Inhale ->
                                Icon.view Icon.Wind

                            InhaleHold ->
                                Icon.view Icon.Pause

                            Exhale ->
                                div [ class "transform-rotate-180" ] [ Icon.view Icon.Wind ]

                            ExhaleHold ->
                                Icon.view Icon.Pause

                    toAriaLabel phaseType =
                        attribute "aria-label" <| phaseTypeToString phaseType

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
                div
                    [ attribute "role" "preparation"
                    , class "max-w-2xl mx-auto"
                    ]
                    [ div
                        [ class "text-center mb-8"
                        ]
                        [ h1 [ class "text-2xl font-semibold text-gray-800" ] [ text "セッション準備" ]
                        , p [ class "text-gray-500 mt-2" ]
                            [ text "呼吸法をカスタマイズして始めましょう"
                            ]
                        ]
                    , breathingMethodControls
                    , div [ class "space-y-8 mt-4" ]
                        [ div [ class "flex items-center space-x-3 bg-gray-50 p-4 rounded-lg" ]
                            [ Icon.view Icon.Timer
                            , input
                                [ attribute "aria-label" "session-duration-input"
                                , onInput InputSessionDuration
                                , type_ "number"
                                , placeholder "セッション時間（分）"
                                , class "w-full border-none bg-transparent focus:ring-0"
                                ]
                                []
                            ]
                        , button
                            ([ attribute "aria-label" "start-session"
                             , class "w-full py-6 text-lg bg-gradient-to-r from-blue-500 to-indigo-500 hover:from-blue-600 hover:to-indigo-600 transition-all duration-300 rounded-lg disabled:opacity-50 grid grid-flow-col gap-2 items-center justify-center"
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
                            [ Icon.view Icon.Play
                            , text "セッションを始める"
                            ]
                        ]
                    , div
                        [ attribute "aria-label" "backdrop"
                        , class "cursor-pointer w-[10px] h-[10px] bg-white"
                        ]
                        []
                    ]

            ModelLoading _ ->
                div []
                    [ text "Loading..." ]
    }
