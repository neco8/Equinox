module Pages.SessionPage exposing
    ( Model, SelectedBreathingMethod(..), init
    , Msg
    , update
    , view
    , subscriptions
    )

{-| このモジュールはセッションページに関するものです。


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
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, PhaseType(..))


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
type alias Model =
    { selectedBreathingMethod : SelectedBreathingMethod
    , timerState : TimerState
    , displayCurrentTime : Time.Posix
    }


{-| フェーズの計算
-}
calculatePhase : Int -> SelectedBreathingMethod -> { phaseType : PhaseType, elapsedMillisecondsInPhase : Int }
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
            inhaleDuration * 1000

        inhaleHoldDurationMs =
            inhaleHoldDuration * 1000

        exhaleDurationMs =
            exhaleDuration * 1000

        exhaleHoldDurationMs =
            exhaleHoldDuration * 1000

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


{-| 選択された呼吸法
-}
type SelectedBreathingMethod
    = Existing BreathingMethod
    | Custom
        { inhaleDuration : InhaleDuration
        , inhaleHoldDuration : InhaleHoldDuration
        , exhaleDuration : ExhaleDuration
        , exhaleHoldDuration : ExhaleHoldDuration
        }


{-| 初期化
-}
init : SelectedBreathingMethod -> ( Model, Cmd Msg )
init selected =
    ( { selectedBreathingMethod = selected
      , timerState = NotStarted
      , displayCurrentTime = Time.millisToPosix 0
      }
    , Task.perform Start Time.now
    )


{-| スタートに関する処理
-}
handleStart : Time.Posix -> Model -> ( Model, Cmd Msg )
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
handlePause : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int } -> Model -> ( Model, Cmd Msg )
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
handleResume : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int, pauseStartTime : Time.Posix } -> Model -> ( Model, Cmd Msg )
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
handleStop : Time.Posix -> { startTime : Time.Posix, totalPausedMilliseconds : Int, pauseStartTime : Time.Posix } -> Model -> ( Model, Cmd Msg )
handleStop now paused model =
    let
        finalDuration =
            Time.posixToMillis now
                - Time.posixToMillis paused.pauseStartTime
    in
    ( { model
        | timerState =
            Completed
                { startTime = paused.startTime
                , totalPausedMilliseconds = paused.totalPausedMilliseconds + finalDuration
                , endTime = now
                }
      }
    , Cmd.none
    )


{-| 時間経過後セッション完了に関する処理

次の画面に遷移する処理

-}
handleNavigateToCompleteSession : Int -> Model -> ( Model, Cmd Msg )
handleNavigateToCompleteSession duration model =
    let
        route =
            case model.selectedBreathingMethod of
                Existing method ->
                    Route.PresetSessionCompletionRoute method.id (Just duration)

                Custom _ ->
                    Route.ManualSessionCompletionRoute (Just duration)
    in
    ( model
    , Task.perform
        (always <| NavigateToRoute route)
        Time.now
    )


{-| アップデート
-}
update : Int -> Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update duration key msg model =
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
            ( model
            , Cmd.none
            )

        ( TickDisplayTime posix, _ ) ->
            if getElapsedMilliseconds model.timerState posix >= duration * 1000 then
                handleNavigateToCompleteSession duration model

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
view : Int -> Model -> Html Msg
view duration model =
    div
        [ attribute "role" "session"
        ]
        [ viewTimer model
        , viewInstruction model
        , viewControls model
        , text <| "総時間: " ++ String.fromInt duration ++ "秒"
        ]


{-| タイマーのビュー
-}
viewTimer : Model -> Html Msg
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
        [ attribute "role" "session-timer"
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
viewInstruction : Model -> Html Msg
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
viewControls : Model -> Html Msg
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
    case model.timerState of
        NotStarted ->
            Time.every 100 TickDisplayTime

        Running _ ->
            Time.every 100 TickDisplayTime

        Paused _ ->
            Time.every 100 TickDisplayTime

        Completed _ ->
            Sub.none
