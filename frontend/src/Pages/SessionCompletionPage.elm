module Pages.SessionCompletionPage exposing
    ( Msg, noOp
    , view
    , update
    , Model, PracticeStyle(..), init
    )

{-|


## Session Completion Page

このモジュールはセッション完了ページに関するものです。


### メッセージ

@docs Msg, noOp


### ビュー

@docs view


### アップデート

@docs update


### モデル

-}

import Browser.Navigation as Nav
import Html exposing (button, div, h2, node, p, span, text)
import Html.Attributes exposing (attribute, class, value)
import Html.Events exposing (onClick)
import Icon
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, PhaseType(..), fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, fromName)
import Types.Session exposing (Duration, Session, fromDuration)
import Types.Statistics exposing (calculateStreak)
import View exposing (View)


{-| メッセージ
-}
type Msg
    = NavigateToRoute Route
    | NextStep
    | NoOp


{-| メッセージ: NoOp

画面更新用に利用される。

-}
noOp : Msg
noOp =
    NoOp


{-| 練習スタイル

手動で設定したBreathingMethodかもしくは既存のBreathingMethodかを選択する

-}
type PracticeStyle
    = ManualPracticeStyle (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)
    | PresetPracticeStyle BreathingMethodId


{-| 正常な手動練習スタイル

手動で設定した場合、各フェーズの秒数がすでに存在する

-}
type alias ValidManualPracticeStyle =
    { inhaleDuration : InhaleDuration
    , inhaleHoldDuration : InhaleHoldDuration
    , exhaleDuration : ExhaleDuration
    , exhaleHoldDuration : ExhaleHoldDuration
    }


{-| 正常な練習スタイル
-}
type ValidPracticeStyle
    = Manual ValidManualPracticeStyle
    | Preset BreathingMethod


{-| validatePracticeStyleの結果

未知という状態が存在する

    type ValidatePracticeStyleResult
        = Valid ValidPracticeStyle
        | Invalid
        | NotYetKnown

-}
type ValidatePracticeStyleResult
    = Valid ValidPracticeStyle (List Session)
    | Invalid
    | NotYetKnown


{-| 練習スタイルを検証する

既存の呼吸法のリストに存在するかを検証する

-}
validatePracticeStyle : RemoteData e { breathingMethods : List BreathingMethod, sessions : List Session } -> PracticeStyle -> ValidatePracticeStyleResult
validatePracticeStyle remote practiceStyle =
    case practiceStyle of
        ManualPracticeStyle minhale minhaleHold mexhale mexhaleHold ->
            case remote of
                Success data ->
                    Just ValidManualPracticeStyle
                        |> Maybe.Extra.andMap minhale
                        |> Maybe.Extra.andMap minhaleHold
                        |> Maybe.Extra.andMap mexhale
                        |> Maybe.Extra.andMap mexhaleHold
                        |> Maybe.map (Manual >> Valid >> (|>) data.sessions)
                        |> Maybe.withDefault Invalid

                Failure _ ->
                    Invalid

                NotAsked ->
                    NotYetKnown

                Loading ->
                    NotYetKnown

        PresetPracticeStyle id ->
            case remote of
                Success data ->
                    case
                        List.Extra.find (.id >> (==) id) data.breathingMethods
                            |> Maybe.map Preset
                    of
                        Just valid ->
                            Valid valid data.sessions

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


{-| Model
-}
type Model
    = ModelInvalid PracticeStyle
    | ModelLoading
        { duration : Duration
        , practiceStyle : PracticeStyle
        }
    | ModelValid InternalModel


{-| ローディングした後に利用されるModel
-}
type alias InternalModel =
    { duration : Duration
    , practiceStyle : ValidPracticeStyle
    , step : Step
    , sessions : List Session
    }


{-| ページ内でのステップ

    type Step
        = Result
        | Streak

-}
type Step
    = Result
    | Streak


{-| 初期化
-}
init : RemoteData e { breathingMethods : List BreathingMethod, sessions : List Session } -> Maybe Duration -> PracticeStyle -> ( Model, Cmd Msg )
init remote mduration practiceStyle =
    case mduration of
        Just duration ->
            -- セッション秒数ページが存在していても、validationをする必要がある。
            case validatePracticeStyle remote practiceStyle of
                Valid valid sessions ->
                    ( ModelValid
                        { duration = duration
                        , practiceStyle = valid
                        , step = Result
                        , sessions = sessions
                        }
                    , Cmd.none
                    )

                Invalid ->
                    ( ModelInvalid practiceStyle
                    , -- ホーム画面に遷移する
                      NavigateToRoute HomeRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now
                    )

                NotYetKnown ->
                    ( ModelLoading
                        { duration = duration
                        , practiceStyle = practiceStyle
                        }
                    , Cmd.none
                    )

        Nothing ->
            -- 完了秒数が存在しない場合、、即座にセッション準備ページに遷移する
            ( ModelInvalid practiceStyle
            , (case practiceStyle of
                PresetPracticeStyle id ->
                    PresetSessionPreparationRoute id

                ManualPracticeStyle _ _ _ _ ->
                    ManualSessionPreparationRoute
              )
                |> NavigateToRoute
                |> always
                |> Task.perform
                |> (|>) Time.now
            )


{-| ビュー
-}
view : Model -> View Msg
view model =
    { nav = Nothing
    , footer = False
    , view =
        case model of
            ModelValid { duration, practiceStyle, step, sessions } ->
                div
                    [ attribute "role" "session-completion"
                    , class "flex flex-col items-center justify-center min-h-full bg-gradient-to-b from-blue-50 to-white p-4"
                    ]
                <|
                    List.singleton <|
                        div [ class "flex flex-col items-center space-y-8 w-full max-w-md" ] <|
                            case step of
                                Result ->
                                    [ div [ class "relative flex items-center justify-center w-24 h-24 rounded-full bg-green-100 text-4xl" ]
                                        [ Icon.view Icon.Medal
                                        ]
                                    , div [ class "flex items-center justify-center space-x-2 text-8xl font-bold text-gray-900" ]
                                        [ div [ class "text-5xl mr-4" ]
                                            [ Icon.view Icon.Timer
                                            ]
                                        , div [ class "align-baseline" ]
                                            [ span
                                                [ attribute "aria-label" "finish-duration"
                                                , value (String.fromInt <| fromDuration duration)
                                                ]
                                                [ text <| String.fromInt <| floor <| (\s -> s / 60) <| toFloat <| fromDuration duration ]
                                            , span [ class "text-4xl" ] [ text "分" ]
                                            ]
                                        ]
                                    , div [ class "space-y-1 w-full" ]
                                        [ div [ class "text-2xl font-thin text-gray-500 text-center" ]
                                            [ text <|
                                                (\m ->
                                                    String.fromInt (fromInhaleDuration m.inhaleDuration)
                                                        ++ "-"
                                                        ++ String.fromInt (fromInhaleHoldDuration m.inhaleHoldDuration)
                                                        ++ "-"
                                                        ++ String.fromInt (fromExhaleDuration m.exhaleDuration)
                                                        ++ "-"
                                                        ++ String.fromInt (fromExhaleHoldDuration m.exhaleHoldDuration)
                                                )
                                                <|
                                                    case practiceStyle of
                                                        Preset preset ->
                                                            { inhaleDuration = preset.inhaleDuration
                                                            , inhaleHoldDuration = preset.inhaleHoldDuration
                                                            , exhaleDuration = preset.exhaleDuration
                                                            , exhaleHoldDuration = preset.exhaleHoldDuration
                                                            }

                                                        Manual manual ->
                                                            { inhaleDuration = manual.inhaleDuration
                                                            , inhaleHoldDuration = manual.inhaleHoldDuration
                                                            , exhaleDuration = manual.exhaleDuration
                                                            , exhaleHoldDuration = manual.exhaleHoldDuration
                                                            }
                                            ]
                                        , case practiceStyle of
                                            Preset method ->
                                                div [ class "text-sm font-thin text-gray-400 text-center" ]
                                                    [ text <| fromName method.name
                                                    ]

                                            Manual _ ->
                                                text ""
                                        ]
                                    , div
                                        [ class "text-center" ]
                                        [ h2 [ class "text-xl font-semibold text-gray-900 mb-2" ] [ text "Excellent work!" ]
                                        , p [ class "text-gray-600" ] [ text "呼吸法セッションを完了しました" ]
                                        ]
                                    , button
                                        [ attribute "aria-label" "next"
                                        , onClick NextStep
                                        , class "w-full py-4 px-6 bg-blue-600 text-white text-lg rounded-xl font-medium hover:bg-blue-700 transition-all transform hover:scale-105 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
                                        ]
                                        [ text "次へ" ]
                                    ]

                                Streak ->
                                    [ node "streak-animation"
                                        [ attribute "streak" <| String.fromInt <| calculateStreak sessions
                                        ]
                                        []
                                    , button
                                        [ attribute "aria-label" "finish"
                                        , onClick (NavigateToRoute StatisticsRoute)
                                        , class "w-full py-4 px-6 bg-green-600 text-white text-lg rounded-xl font-medium hover:bg-green-700 transition-all transform hover:scale-105 focus:outline-none focus:ring-2 focus:ring-green-500 focus:ring-offset-2"
                                        ]
                                        [ text "完了" ]
                                    ]

            ModelInvalid _ ->
                text "durationが存在していません。"

            ModelLoading _ ->
                text "loading"
    }


{-| アップデート
-}
update : RemoteData e { breathingMethods : List BreathingMethod, sessions : List Session } -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update remote key msg model =
    case model of
        ModelValid valid ->
            updateInternal key msg valid
                |> Tuple.mapFirst ModelValid

        ModelInvalid practiceStyle ->
            ( ModelInvalid practiceStyle
            , NavigateToRoute HomeRoute
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        ModelLoading { duration, practiceStyle } ->
            init remote (Just duration) practiceStyle


{-| 内部で利用するアップデート
-}
updateInternal : Nav.Key -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal key msg model =
    case msg of
        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route) )

        NextStep ->
            case model.step of
                Result ->
                    ( { model | step = Streak }
                    , Cmd.none
                    )

                Streak ->
                    ( model
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )
