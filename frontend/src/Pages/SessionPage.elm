module Pages.SessionPage exposing
    ( Model, SelectedBreathingMethod(..), init
    , Msg, noOp
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

@docs Msg, noOp


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
import Icon
import JS.Ports as Ports
import JS.Storage.StorageQueryDSL exposing (Query(..))
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, PhaseType(..), fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration)
import Types.Session exposing (Duration, Session, fromDuration, toDuration)
import Uuid exposing (Uuid)
import View exposing (View)


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
    | GetSessionId (Uuid -> Session)
    | GotSessionId (Uuid -> Session) Uuid
    | NoOp


{-| メッセージ: NoOp

画面更新用のメッセージ

-}
noOp : Msg
noOp =
    NoOp


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
            createSession model.selectedBreathingMethod d now
                |> GetSessionId
                |> always
                |> Task.perform
                |> (|>) Time.now

        Nothing ->
            -- TODO: セッションより超過するか、短すぎるか。短すぎるときに、時間にならないですよ、というのを追加してあげる。
            Cmd.none
    )


{-| 時間経過後セッション完了に関する処理

  - セッションの結果を保存する処理
  - セッションを再度取得する処理
  - 次の画面に遷移する処理

-}
handleCompletion : Duration -> InternalModel -> Session -> Cmd Msg
handleCompletion duration model session =
    let
        route =
            case model.selectedBreathingMethod of
                Existing method ->
                    Route.PresetSessionCompletionRoute method.id (Just duration)

                Custom custom ->
                    Route.ManualSessionCompletionRoute (Just duration)
                        (Just custom.inhaleDuration)
                        (Just custom.inhaleHoldDuration)
                        (Just custom.exhaleDuration)
                        (Just custom.exhaleHoldDuration)
    in
    Cmd.batch
        [ Task.perform
            (always <| NavigateToRoute route)
            Time.now
        , Ports.saveSessionValue session
        , Ports.loadQuery GetAllSessions
        ]


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
update : RemoteData e (List BreathingMethod) -> Maybe Duration -> Nav.Key -> Msg -> Model -> (Msg -> msg) -> Uuid.Registry msg -> ( Model, Cmd msg, Uuid.Registry msg )
update remote duration key msg model toMsg registry =
    case model of
        ModelLoading selectedBreathingMethod ->
            let
                breathingMethodState =
                    validateSelectedBreathingMethod remote selectedBreathingMethod

                redirectToHomeCmd =
                    redirectToHome breathingMethodState
                        |> Cmd.map toMsg

                redirectToPreparationCmd =
                    Cmd.map toMsg <|
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
                        , cmd |> Cmd.map toMsg
                        ]
                    , registry
                    )

                _ ->
                    ( ModelLoading selectedBreathingMethod
                    , Cmd.batch [ redirectToPreparationCmd, redirectToHomeCmd ]
                    , registry
                    )

        ModelLoaded loaded ->
            case duration of
                Just d ->
                    let
                        ( newInternalModel, cmd, newRegistry ) =
                            updateInternal d key msg loaded toMsg registry
                    in
                    ( ModelLoaded newInternalModel
                    , cmd
                    , newRegistry
                    )

                Nothing ->
                    ( ModelLoaded loaded
                    , redirectToPreparation duration loaded.selectedBreathingMethod
                        |> Cmd.map toMsg
                    , registry
                    )


{-| レジストリをタプルに追加するためのヘルパー関数
-}
withRegistry : registry -> ( model, cmd ) -> ( model, cmd, registry )
withRegistry registry ( m, c ) =
    ( m, c, registry )


{-| セッションを作成する
-}
createSession : ValidSelectedBreathingMethod -> Duration -> Time.Posix -> Uuid -> Session
createSession selectedBreathingMethod duration posix =
    let
        method =
            case selectedBreathingMethod of
                Existing m ->
                    { id = Just m.id
                    , name = Just m.name
                    , inhaleDuration = m.inhaleDuration
                    , inhaleHoldDuration = m.inhaleHoldDuration
                    , exhaleDuration = m.exhaleDuration
                    , exhaleHoldDuration = m.exhaleHoldDuration
                    }

                Custom custom ->
                    { id = Nothing
                    , name = Nothing
                    , inhaleDuration = custom.inhaleDuration
                    , inhaleHoldDuration = custom.inhaleHoldDuration
                    , exhaleDuration = custom.exhaleDuration
                    , exhaleHoldDuration = custom.exhaleHoldDuration
                    }
    in
    \id ->
        Session
            id
            method.inhaleDuration
            method.inhaleHoldDuration
            method.exhaleDuration
            method.exhaleHoldDuration
            method.id
            method.name
            duration
            -- 最後まで終わったので、durationをそのまま使う
            posix


{-| 内部で利用されているアップデート(リダイレクト考慮なし)
-}
updateInternal : Duration -> Nav.Key -> Msg -> InternalModel -> (Msg -> msg) -> Uuid.Registry msg -> ( InternalModel, Cmd msg, Uuid.Registry msg )
updateInternal duration key msg model toMsg registry =
    case ( msg, model.timerState ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none, registry )

        ( Start now, NotStarted ) ->
            handleStart now model
                |> Tuple.mapSecond (Cmd.map toMsg)
                |> withRegistry registry

        ( Start _, _ ) ->
            ( model, Cmd.none, registry )

        ( ClickPauseButton, Running _ ) ->
            ( model, Task.perform (Pause >> toMsg) Time.now, registry )

        ( ClickPauseButton, _ ) ->
            ( model, Cmd.none, registry )

        ( Pause now, Running running ) ->
            handlePause now running model
                |> Tuple.mapSecond (Cmd.map toMsg)
                |> withRegistry registry

        ( Pause _, _ ) ->
            ( model, Cmd.none, registry )

        ( ClickResumeButton, Paused _ ) ->
            ( model, Task.perform (Resume >> toMsg) Time.now, registry )

        ( ClickResumeButton, _ ) ->
            ( model, Cmd.none, registry )

        ( Resume now, Paused paused ) ->
            handleResume now paused model
                |> Tuple.mapSecond (Cmd.map toMsg)
                |> withRegistry registry

        ( Resume _, _ ) ->
            ( model, Cmd.none, registry )

        ( ClickStopButton, Paused _ ) ->
            ( model, Task.perform (Stop >> toMsg) Time.now, registry )

        ( ClickStopButton, _ ) ->
            ( model, Cmd.none, registry )

        ( Stop now, Paused paused ) ->
            handleStop now paused model
                |> Tuple.mapSecond (Cmd.map toMsg)
                |> withRegistry registry

        ( Stop _, _ ) ->
            ( model, Cmd.none, registry )

        ( TickDisplayTime posix, _ ) ->
            if getElapsedMilliseconds model.timerState posix >= fromDuration duration * 1000 then
                ( model
                , createSession model.selectedBreathingMethod duration posix
                    |> GetSessionId
                    |> toMsg
                    |> always
                    |> Task.perform
                    |> (|>) Time.now
                , registry
                )

            else
                ( { model | displayCurrentTime = posix }, Cmd.none, registry )

        ( NavigateToRoute route, _ ) ->
            ( model, Nav.pushUrl key <| Route.toString route, registry )

        ( GetSessionId f, _ ) ->
            let
                ( newRegistry, effect, maybeMsg ) =
                    Uuid.uuidGenerate "session/complete-session" (GotSessionId f >> toMsg)
                        |> Uuid.update
                        |> (|>) registry

                cmd =
                    Cmd.batch
                        [ maybeMsg
                            |> Maybe.map (always >> Task.perform >> (|>) Time.now)
                            |> Maybe.withDefault Cmd.none
                        , Uuid.effectToCmd Ports.generateUuidValue effect
                        ]
            in
            ( model
            , cmd
            , newRegistry
            )

        ( GotSessionId f id, _ ) ->
            ( model
            , handleCompletion (f id).duration model (f id)
                |> Cmd.map toMsg
            , registry
            )


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
view : Maybe Duration -> Model -> View Msg
view _ model =
    { nav = False
    , footer = False
    , view =
        case model of
            ModelLoading _ ->
                div [] [ text "breathing method loading..." ]

            ModelLoaded loaded ->
                let
                    { inhaleDuration, inhaleHoldDuration, exhaleDuration, exhaleHoldDuration } =
                        case loaded.selectedBreathingMethod of
                            Existing m ->
                                { inhaleDuration = m.inhaleDuration
                                , inhaleHoldDuration = m.inhaleHoldDuration
                                , exhaleDuration = m.exhaleDuration
                                , exhaleHoldDuration = m.exhaleHoldDuration
                                }

                            Custom m ->
                                { inhaleDuration = m.inhaleDuration
                                , inhaleHoldDuration = m.inhaleHoldDuration
                                , exhaleDuration = m.exhaleDuration
                                , exhaleHoldDuration = m.exhaleHoldDuration
                                }
                in
                div
                    [ attribute "role" "session"
                    , class "relative flex flex-col items-center justify-center h-full bg-white text-gray-900 p-4 gap-60"
                    ]
                    [ viewTimer loaded
                    , div [ class "absolute" ]
                        [ div [ class "relative flex items-center justify-center mb-4 h-80" ]
                            [ node "breathing-animation"
                                [ attribute "inhale" <|
                                    (String.fromInt <| fromInhaleDuration inhaleDuration)
                                , attribute "inhale-hold" <|
                                    (String.fromInt <| fromInhaleHoldDuration inhaleHoldDuration)
                                , attribute "exhale" <|
                                    (String.fromInt <| fromExhaleDuration exhaleDuration)
                                , attribute "exhale-hold" <|
                                    (String.fromInt <| fromExhaleHoldDuration exhaleHoldDuration)
                                , attribute "paused" <|
                                    case loaded.timerState of
                                        Paused _ ->
                                            "true"

                                        _ ->
                                            "false"
                                ]
                                []
                            , viewInstruction loaded
                                [ class "absolute text-2xl"
                                ]
                            ]
                        ]
                    , viewControls loaded
                    ]
    }


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
        , class "text-6xl font-mono z-10 p-8 relative"
        ]
        [ div [ class "absolute -inset-6 bg-blue-100/20 blur-2xl" ] []
        , span [ class "relative" ] [ text (minutes ++ ":" ++ seconds) ]
        ]


{-| 指示文のビュー
-}
viewInstruction : InternalModel -> List (Attribute msg) -> Html msg
viewInstruction model attr =
    let
        elapsedMilliseconds =
            getElapsedMilliseconds model.timerState model.displayCurrentTime

        { phaseType } =
            calculatePhase elapsedMilliseconds model.selectedBreathingMethod
    in
    article
        (attribute "aria-label" "session-instruction"
            :: attr
        )
        [ text <| instructionText phaseType
        ]


{-| コントロールのビュー
-}
viewControls : InternalModel -> Html Msg
viewControls model =
    let
        buttonClass =
            class "p-4 bg-gray-50/70 hover:bg-gray-200/70 backdrop-blur-sm rounded-full transition-colors"
    in
    div [ class "flex justify-center gap-8 z-10" ] <|
        case model.timerState of
            NotStarted ->
                [ text "loading..." ]

            Running _ ->
                [ button
                    [ attribute "aria-label" "pause"
                    , onClick ClickPauseButton
                    , buttonClass
                    ]
                    [ Icon.view Icon.Pause ]
                ]

            Paused _ ->
                [ button
                    [ attribute "aria-label" "resume"
                    , onClick ClickResumeButton
                    , buttonClass
                    ]
                    [ Icon.view Icon.Play ]
                , button
                    [ attribute "aria-label" "stop"
                    , onClick ClickStopButton
                    , buttonClass
                    ]
                    [ Icon.view Icon.Stop ]
                ]

            Completed _ ->
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
