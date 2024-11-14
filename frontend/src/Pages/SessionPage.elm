module Pages.SessionPage exposing
    ( Model, SelectedBreathingMethod(..), init
    , Msg
    , update
    , view
    , subscriptions
    )

{-|


## Session Page

このモジュールはセッションページに関するものです。


### モデル

@docs Model, SelectedBreathingMethod, init


### メッセージ

@docs Msg


### アップデート

@docs update


### ビュー

@docs view


### サブスクリプション

@docs subscriptions


### TODO

-- [ ] TODO: セッションが完了したとき、セッションを保存する。ほうがいいのでは？？？でも、後々でいいや。完了画面が、sessionIdによって紐づいているかもしれない。
-- [ ] TODO: セッションが途中で中断されたとき、中断しますか？と聞いたあとに保存し中断（statisticsへ）。中断できない時間ならば、中断できない旨を表示し、中断してホームへ。
-- [ ] TODO: GSAPを使ってアニメーションを追加する。

-}

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, PhaseType(..), fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration)
import Types.Session exposing (Duration, fromDuration, toDuration)


{-| タイマーを管理するための状態
-}
type TimerState
    = NotStarted
    | Running
        { startTime : Time.Posix
        , totalPausedMilliseconds : Int
        }
    | Paused
        { startTime : Time.Posix
        , totalPausedMilliseconds : Int
        , pauseStartTime : Time.Posix
        }
    | Completed
        { startTime : Time.Posix
        , totalPausedMilliseconds : Int
        , endTime : Time.Posix
        }


{-| モデル
-}
type Model
    = ModelLoading SelectedBreathingMethod
    | ModelLoaded InternalModel


{-| 内部モデル

ここでは、すでにすべてが正しい状態で入っているモデルを表現している。

-}
type alias InternalModel =
    { selectedBreathingMethod : ValidSelectedBreathingMethod
    , timerState : TimerState
    , displayCurrentTime : Time.Posix
    }


{-| フェーズの計算
-}
calculatePhase : Int -> ValidSelectedBreathingMethod -> { phaseType : PhaseType, elapsedMillisecondsInPhase : Int }
calculatePhase elapsedMilliseconds method =
    let
        { inhaleDuration, inhaleHoldDuration, exhaleDuration, exhaleHoldDuration } =
            case method of
                Existing m ->
                    { inhaleDuration = m.inhaleDuration
                    , inhaleHoldDuration = m.inhaleHoldDuration
                    , exhaleDuration = m.exhaleDuration
                    , exhaleHoldDuration = m.exhaleHoldDuration
                    }

                Custom m ->
                    m

        inhaleDurationMs =
            fromInhaleDuration inhaleDuration * 1000

        inhaleHoldDurationMs =
            fromInhaleHoldDuration inhaleHoldDuration * 1000

        exhaleDurationMs =
            fromExhaleDuration exhaleDuration * 1000

        exhaleHoldDurationMs =
            fromExhaleHoldDuration exhaleHoldDuration * 1000

        totalCycleDuration =
            inhaleDurationMs + inhaleHoldDurationMs + exhaleDurationMs + exhaleHoldDurationMs

        timeInCycle =
            modBy totalCycleDuration elapsedMilliseconds
    in
    if timeInCycle < inhaleDurationMs then
        { phaseType = Inhale
        , elapsedMillisecondsInPhase = timeInCycle
        }

    else if timeInCycle < inhaleDurationMs + inhaleHoldDurationMs then
        { phaseType = InhaleHold
        , elapsedMillisecondsInPhase = timeInCycle - inhaleDurationMs
        }

    else if timeInCycle < inhaleDurationMs + inhaleHoldDurationMs + exhaleDurationMs then
        { phaseType = Exhale
        , elapsedMillisecondsInPhase = timeInCycle - (inhaleDurationMs + inhaleHoldDurationMs)
        }

    else
        { phaseType = ExhaleHold
        , elapsedMillisecondsInPhase = timeInCycle - (inhaleDurationMs + inhaleHoldDurationMs + exhaleDurationMs)
        }


{-| 指示文を取得する
-}
instructionText : PhaseType -> String
instructionText phaseType =
    case phaseType of
        Inhale ->
            "吸う"

        InhaleHold ->
            "止める"

        Exhale ->
            "吐く"

        ExhaleHold ->
            "止める"


{-| メッセージ
-}
type Msg
    = Start Time.Posix
    | ClickPauseButton
    | Pause Time.Posix
    | ClickResumeButton
    | Resume Time.Posix
    | ClickStopButton
    | Stop Time.Posix
    | TickDisplayTime Time.Posix
    | NavigateToRoute Route


{-| ページを呼び出す際選択された呼吸法

ただし、まだ指定された呼吸法などが正しいか否かが不明なため、後々ValidSelectedBreathingMethodに変換する必要がある。

-}
type SelectedBreathingMethod
    = PresetBreathingMethod BreathingMethodId
    | CustomBreathingMethod
        { inhaleDuration : Maybe InhaleDuration
        , inhaleHoldDuration : Maybe InhaleHoldDuration
        , exhaleDuration : Maybe ExhaleDuration
        , exhaleHoldDuration : Maybe ExhaleHoldDuration
        }


{-| カスタム呼吸法で正確に秒数が入力されている型
-}
type alias CustomValidBreathingMethod =
    { inhaleDuration : InhaleDuration
    , inhaleHoldDuration : InhaleHoldDuration
    , exhaleDuration : ExhaleDuration
    , exhaleHoldDuration : ExhaleHoldDuration
    }


{-| 選択された呼吸法
-}
type ValidSelectedBreathingMethod
    = Existing BreathingMethod
    | Custom CustomValidBreathingMethod


{-| 初期化
-}
init : RemoteData e (List BreathingMethod) -> Maybe Duration -> SelectedBreathingMethod -> ( Model, Cmd Msg )
init remote duration selected =
    let
        breathingMethodState =
            validateSelectedBreathingMethod remote selected

        redirectToHomeCmd =
            redirectToHome breathingMethodState

        redirectToPreparationCmd =
            case breathingMethodState of
                Valid valid ->
                    redirectToPreparation duration valid

                _ ->
                    Cmd.none
    in
    case breathingMethodState of
        Valid valid ->
            let
                ( newModel, cmd ) =
                    initInternal valid
            in
            ( ModelLoaded newModel
            , Cmd.batch
                [ redirectToPreparationCmd
                , redirectToHomeCmd
                , cmd
                ]
            )

        _ ->
            ( ModelLoading selected
            , Cmd.batch [ redirectToPreparationCmd, redirectToHomeCmd ]
            )


{-| 初期化
-}
initInternal : ValidSelectedBreathingMethod -> ( InternalModel, Cmd Msg )
initInternal selected =
    ( { selectedBreathingMethod = selected
      , timerState = NotStarted
      , displayCurrentTime = Time.millisToPosix 0
      }
    , Task.perform Start Time.now
    )


{-| スタートに関する処理
-}
handleStart : Time.Posix -> InternalModel -> ( InternalModel, Cmd Msg )
handleStart now model =
    ( { model
        | timerState =
            Running { startTime = now, totalPausedMilliseconds = 0 }
        , displayCurrentTime = now
      }
    , Cmd.none
    )


{-| 一時停止に関する処理
-}
handlePause : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int } -> InternalModel -> ( InternalModel, Cmd Msg )
handlePause now running model =
    ( { model
        | timerState =
            Paused
                { startTime = running.startTime
                , totalPausedMilliseconds = running.totalPausedMilliseconds
                , pauseStartTime = now
                }
      }
    , Cmd.none
    )


{-| 再開に関する処理
-}
handleResume : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int, pauseStartTime : Time.Posix } -> InternalModel -> ( InternalModel, Cmd Msg )
handleResume now paused model =
    let
        pauseDuration =
            Time.posixToMillis now
                - Time.posixToMillis paused.pauseStartTime
    in
    ( { model
        | timerState =
            Running
                { startTime = paused.startTime
                , totalPausedMilliseconds = paused.totalPausedMilliseconds + pauseDuration
                }
      }
    , Cmd.none
    )


{-| 停止に関する処理

タイマー状態を完了に変更する

-}
handleStop : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int, pauseStartTime : Time.Posix } -> InternalModel -> ( InternalModel, Cmd Msg )
handleStop now paused model =
    let
        finalDuration =
            Time.posixToMillis now
                - Time.posixToMillis paused.pauseStartTime

        timerState =
            Completed
                { startTime = paused.startTime
                , totalPausedMilliseconds = paused.totalPausedMilliseconds + finalDuration
                , endTime = now
                }

        elapsedMilliseconds =
            getElapsedMilliseconds timerState now
    in
    ( { model
        | timerState =
            timerState
      }
    , case toDuration <| elapsedMilliseconds // 1000 of
        Just d ->
            handleNavigateToCompleteSession d model

        Nothing ->
            -- TODO: セッションより超過するか、短すぎるか。短すぎるときに、時間にならないですよ、というのを追加してあげる。
            Cmd.none
    )


{-| 時間経過後セッション完了に関する処理

次の画面に遷移する処理

-}
handleNavigateToCompleteSession : Duration -> InternalModel -> Cmd Msg
handleNavigateToCompleteSession duration model =
    let
        route =
            case model.selectedBreathingMethod of
                Existing method ->
                    Route.PresetSessionCompletionRoute method.id (Just duration)

                Custom _ ->
                    Route.ManualSessionCompletionRoute (Just duration)
    in
    Task.perform
        (always <| NavigateToRoute route)
        Time.now


{-| ストレージの呼吸法がロード中なのか、それとも不正な状態なのかを表す型
-}
type BreathingMethodState
    = BreathingMethodLoading
    | InvalidBreathingMethodId
    | InvalidCustomBreathingMethodDuration
    | Valid ValidSelectedBreathingMethod


{-| 選択された呼吸法が正しいかどうかを検証する関数

これは既存の呼吸法を選択するときにしか使われない。

カスタムの場合は、すでに正しい値が入っているはず。

-}
validateSelectedBreathingMethod : RemoteData e (List BreathingMethod) -> SelectedBreathingMethod -> BreathingMethodState
validateSelectedBreathingMethod remote selected =
    case selected of
        PresetBreathingMethod id ->
            case RemoteData.map (List.Extra.find (.id >> (==) id)) remote of
                Success (Just method) ->
                    Valid (Existing method)

                Success Nothing ->
                    -- すでに呼吸法がすべて存在しているのに呼吸法が存在しない場合、不正であるのでホーム画面へリダイレクトする
                    -- TODO: ホーム画面へリダイレクトした後、エラーメッセージを表示する
                    InvalidBreathingMethodId

                Failure _ ->
                    -- TODO: 呼吸法取得に失敗したので、いずれエラーメッセージを表示する
                    InvalidBreathingMethodId

                Loading ->
                    BreathingMethodLoading

                NotAsked ->
                    BreathingMethodLoading

        CustomBreathingMethod custom ->
            case
                Just CustomValidBreathingMethod
                    |> Maybe.Extra.andMap custom.inhaleDuration
                    |> Maybe.Extra.andMap custom.inhaleHoldDuration
                    |> Maybe.Extra.andMap custom.exhaleDuration
                    |> Maybe.Extra.andMap custom.exhaleHoldDuration
            of
                Just valid ->
                    Valid (Custom valid)

                Nothing ->
                    InvalidCustomBreathingMethodDuration


{-| 準備画面にリダイレクトする関数

セッションの秒数が存在しない場合、準備画面にリダイレクトする。

ただし、既存の呼吸法で、かつ呼吸法が存在しない場合はそもそも不正なため、、ホーム画面へリダイレクトする必要がある。このリダイレクトは、前提として呼吸法が存在する上での関数である。

-}
redirectToPreparation : Maybe Duration -> ValidSelectedBreathingMethod -> Cmd Msg
redirectToPreparation duration selected =
    case duration of
        Nothing ->
            case selected of
                Existing method ->
                    Route.PresetSessionPreparationRoute method.id
                        |> NavigateToRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now

                Custom _ ->
                    Route.ManualSessionPreparationRoute
                        |> NavigateToRoute
                        |> always
                        |> Task.perform
                        |> (|>) Time.now

        Just _ ->
            Cmd.none


{-| ホーム画面にリダイレクトする関数

呼吸法IDに不正があり、準備画面に戻ることも意味がない場合、ホーム画面にリダイレクトする。

準備画面へのリダイレクトと、根本的なBreathingMethodのStateで条件分岐しているので、2回リダイレクトしてしまってどちらの画面に行くかがわからないということはない。

  - [ ] TODO: ここで、エラーメッセージを表示するために、 #92 のグローバルのエラーメッセージ対応にて、エラーメッセージを表示するようにする。

-}
redirectToHome : BreathingMethodState -> Cmd Msg
redirectToHome bs =
    case bs of
        InvalidBreathingMethodId ->
            NavigateToRoute HomeRoute
                |> always
                |> Task.perform
                |> (|>) Time.now

        InvalidCustomBreathingMethodDuration ->
            -- Manual準備画面へリダイレクトする
            NavigateToRoute ManualSessionPreparationRoute
                |> always
                |> Task.perform
                |> (|>) Time.now

        Valid _ ->
            Cmd.none

        BreathingMethodLoading ->
            Cmd.none


{-| アップデート
-}
update : RemoteData e (List BreathingMethod) -> Maybe Duration -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update remote duration key msg model =
    case model of
        ModelLoading selectedBreathingMethod ->
            let
                breathingMethodState =
                    validateSelectedBreathingMethod remote selectedBreathingMethod

                redirectToHomeCmd =
                    redirectToHome breathingMethodState

                redirectToPreparationCmd =
                    case breathingMethodState of
                        Valid selected ->
                            redirectToPreparation duration selected

                        _ ->
                            Cmd.none
            in
            case breathingMethodState of
                Valid selected ->
                    let
                        ( newModel, cmd ) =
                            initInternal selected
                    in
                    ( ModelLoaded newModel
                    , Cmd.batch
                        [ redirectToPreparationCmd
                        , redirectToHomeCmd
                        , cmd
                        ]
                    )

                _ ->
                    ( ModelLoading selectedBreathingMethod
                    , Cmd.batch [ redirectToPreparationCmd, redirectToHomeCmd ]
                    )

        ModelLoaded loaded ->
            case duration of
                Just d ->
                    let
                        ( newInternalModel, cmd ) =
                            updateInternal d key msg loaded
                    in
                    ( ModelLoaded newInternalModel, cmd )

                Nothing ->
                    ( ModelLoaded loaded, redirectToPreparation duration loaded.selectedBreathingMethod )


{-| 内部で利用されているアップデート(リダイレクト考慮なし)
-}
updateInternal : Duration -> Nav.Key -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal duration key msg model =
    case ( msg, model.timerState ) of
        ( Start now, NotStarted ) ->
            handleStart now model

        ( Start _, _ ) ->
            ( model, Cmd.none )

        ( ClickPauseButton, Running _ ) ->
            ( model, Task.perform Pause Time.now )

        ( ClickPauseButton, _ ) ->
            ( model, Cmd.none )

        ( Pause now, Running running ) ->
            handlePause now running model

        ( Pause _, _ ) ->
            ( model, Cmd.none )

        ( ClickResumeButton, Paused _ ) ->
            ( model, Task.perform Resume Time.now )

        ( ClickResumeButton, _ ) ->
            ( model, Cmd.none )

        ( Resume now, Paused paused ) ->
            handleResume now paused model

        ( Resume _, _ ) ->
            ( model, Cmd.none )

        ( ClickStopButton, Paused _ ) ->
            ( model, Task.perform Stop Time.now )

        ( ClickStopButton, _ ) ->
            ( model, Cmd.none )

        ( Stop now, Paused paused ) ->
            handleStop now paused model

        ( Stop _, _ ) ->
            ( model, Cmd.none )

        ( TickDisplayTime posix, _ ) ->
            if getElapsedMilliseconds model.timerState posix >= fromDuration duration * 1000 then
                ( model, handleNavigateToCompleteSession duration model )

            else
                ( { model | displayCurrentTime = posix }, Cmd.none )

        ( NavigateToRoute route, _ ) ->
            ( model, Nav.pushUrl key <| Route.toString route )


{-| 経過時間をミリ秒で取得する
-}
getElapsedMilliseconds : TimerState -> Time.Posix -> Int
getElapsedMilliseconds timerState displayCurrentTime =
    case timerState of
        NotStarted ->
            0

        Running state ->
            Time.posixToMillis displayCurrentTime
                - Time.posixToMillis state.startTime
                - state.totalPausedMilliseconds

        Paused state ->
            Time.posixToMillis state.pauseStartTime
                - Time.posixToMillis state.startTime
                - state.totalPausedMilliseconds

        Completed state ->
            Time.posixToMillis state.endTime
                - Time.posixToMillis state.startTime
                - state.totalPausedMilliseconds


{-| ビュー
-}
view : Maybe Duration -> Model -> Html Msg
view mduration model =
    case model of
        ModelLoading _ ->
            div [] [ text "loading..." ]

        ModelLoaded loaded ->
            div
                [ attribute "role" "session"
                ]
                [ viewTimer loaded
                , viewInstruction loaded
                , viewControls loaded
                , case mduration of
                    Just duration ->
                        text <| "総時間: " ++ String.fromInt (fromDuration duration) ++ "秒"

                    Nothing ->
                        text "redirecting..."
                ]


{-| タイマーのビュー
-}
viewTimer : InternalModel -> Html Msg
viewTimer model =
    let
        elapsedSeconds =
            getElapsedMilliseconds model.timerState model.displayCurrentTime // 1000

        minutes =
            String.fromInt (elapsedSeconds // 60)
                |> String.padLeft 2 '0'

        seconds =
            String.fromInt (remainderBy 60 elapsedSeconds)
                |> String.padLeft 2 '0'
    in
    div
        [ attribute "role" "timer"
        , attribute "aria-label" "session-timer"
        ]
        [ text (minutes ++ ":" ++ seconds) ]


{-| 時間をフォーマットする
-}
formatFloat : Int -> Float -> String
formatFloat digits num =
    let
        multiplier =
            10.0 ^ toFloat digits

        rounded =
            round (num * multiplier)
                |> toFloat
                |> (\n -> n / multiplier)

        formatDecimal n =
            let
                parts =
                    String.split "." (String.fromFloat n)

                intPart =
                    Maybe.withDefault "0" (List.head parts)

                decPart =
                    Maybe.withDefault "" (List.head (List.drop 1 parts))

                paddedDecPart =
                    decPart ++ String.repeat (digits - String.length decPart) "0"
            in
            if digits > 0 then
                intPart ++ "." ++ paddedDecPart

            else
                intPart
    in
    formatDecimal rounded


{-| 指示文のビュー
-}
viewInstruction : InternalModel -> Html Msg
viewInstruction model =
    let
        elapsedMilliseconds =
            getElapsedMilliseconds model.timerState model.displayCurrentTime

        { phaseType, elapsedMillisecondsInPhase } =
            calculatePhase elapsedMilliseconds model.selectedBreathingMethod
    in
    article
        [ attribute "aria-label" "session-instruction"
        ]
        [ text <| instructionText phaseType
        , text <| (formatFloat 1 << (\n -> n / 1000) << toFloat) elapsedMillisecondsInPhase
        ]


{-| コントロールのビュー
-}
viewControls : InternalModel -> Html Msg
viewControls model =
    case model.timerState of
        NotStarted ->
            div []
                [ text "loading..." ]

        Running _ ->
            div []
                [ button
                    [ attribute "aria-label" "pause"
                    , onClick ClickPauseButton
                    ]
                    [ text "一時停止" ]
                ]

        Paused _ ->
            div []
                [ button
                    [ attribute "aria-label" "resume"
                    , onClick ClickResumeButton
                    ]
                    [ text "再開" ]
                , button
                    [ attribute "aria-label" "stop"
                    , onClick ClickStopButton
                    ]
                    [ text "停止" ]
                ]

        Completed _ ->
            div []
                []


{-| サブスクリプション
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ModelLoading _ ->
            Sub.none

        ModelLoaded internalModel ->
            case internalModel.timerState of
                NotStarted ->
                    Time.every 100 TickDisplayTime

                Running _ ->
                    Time.every 100 TickDisplayTime

                Paused _ ->
                    Time.every 100 TickDisplayTime

                Completed _ ->
                    Time.every 100 TickDisplayTime
