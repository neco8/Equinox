module Main exposing (main)

{-|


## Main Module

このモジュールはEntrypointです。複数のページをまとめ、中央のModelの管理も行います。


### TODO

  - [ ] TODO: 初期表示にて、アニメーションローディング画面を追加する
  - [ ] TODO: 数値入力input達について、validationとdisabledを管理する
  - [ ] TODO: ReceiveQueryResultなど、Queryを送る、受け取る関係のものはより愚直な形へリファクタリングする
  - [ ] TODO: duration系統を、すべて秒単位であることがわかりやすいように、claudeの仕様・e2e・frontendをすべて修正する
  - [ ] TODO: Streakを計算する関数を追加する

-}

import Browser
import Browser.Navigation as Nav
import Config exposing (Config)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon
import JS.Codec exposing (posixDecoder)
import JS.Ports as Ports
import JS.Storage.QueryError exposing (QueryError)
import JS.Storage.QueryResult as QueryResult exposing (QueryResult)
import JS.Storage.StorageQueryDSL as Query
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as DE
import Maybe.Extra
import Pages.BreathingMethodPage as BreathingMethodPage
import Pages.SessionCompletionPage as SessionCompletionPage
import Pages.SessionPage as SessionPage exposing (subscriptions, view)
import Pages.SessionPreparationPage as SessionPreparationPage exposing (PracticeStyle(..))
import Pages.SourceSelectionPage as SourceSelectionPage
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, PhaseType(..))
import Types.Category exposing (Category, fromTitle)
import Types.Session exposing (Duration, Session)
import Types.Statistics exposing (calculateFromSessions, calculateRecentFromSessions, recentDaysThreshold)
import Url
import Uuid
import View exposing (View)


{-| Model
-}
type alias Model =
    { key : Nav.Key
    , now : Time.Posix
    , config : Config
    , currentPage : Page
    , uuidRegistry : Uuid.Registry Msg
    , uuids : List Uuid.Uuid
    , breathingMethods : RemoteData () (List BreathingMethod) -- 暫定的にerrorは()でおく
    , categories : RemoteData () (List Category) -- 暫定的にerrorは()でおく
    , sessions : RemoteData () (List Session) -- 暫定的にerrorは()でおく
    }


{-| ページの種類
-}
type Page
    = HomePage
    | PresetSessionPreparationPage SessionPreparationPage.Model
    | ManualSessionPreparationPage SessionPreparationPage.Model
    | PresetSessionPage (Maybe Duration) SessionPage.Model
    | ManualSessionPage (Maybe Duration) SessionPage.Model
    | PresetSessionCompletionPage SessionCompletionPage.Model
    | ManualSessionCompletionPage SessionCompletionPage.Model
    | StatisticsPage
    | SettingsPage
    | SourceSelectionPage SourceSelectionPage.Model
    | BreathingMethodEditPage BreathingMethodPage.Model
    | BreathingMethodAddPage BreathingMethodPage.Model
    | NotFoundPage


{-| フラッグ
-}
type alias Flags =
    { now : Time.Posix
    , environment : Config.Environment
    }


{-| フラッグの初期値
-}
defaultFlags : Flags
defaultFlags =
    { now = Time.millisToPosix 0
    , environment = Config.defaultEnvironment
    }


{-| フラッグのデコーダー
-}
flagsDecoder : Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> DE.andMap (D.field "now" posixDecoder)
        |> DE.andMap (D.field "environment" Config.environmentDecoder)


{-| Modelを初期化する
-}
init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url key =
    let
        flags =
            D.decodeValue flagsDecoder flagsValue
                |> Result.withDefault defaultFlags

        model : Model
        model =
            { key = key
            , now = flags.now
            , config = Config.config flags.environment
            , currentPage = NotFoundPage
            , uuidRegistry = Uuid.initialRegistry
            , uuids = []
            , breathingMethods = NotAsked
            , categories = NotAsked
            , sessions = NotAsked
            }

        initialQueries =
            [ Query.GetAllBreathingMethods
            , Query.GetAllCategories
            , Query.GetAllSessions
            ]

        cmd =
            Cmd.batch <|
                List.map
                    (\query ->
                        Ports.loadQuery query
                    )
                    initialQueries
    in
    updateModelFromUrl model url
        |> (\( updatedModel, c ) -> ( updatedModel, Cmd.batch [ c, cmd ] ))


{-| URLからRouteを解析し、Modelを更新する
-}
updateModelFromUrl : Model -> Url.Url -> ( Model, Cmd Msg )
updateModelFromUrl model url =
    case Route.fromUrl url of
        Just route ->
            initializePage model route
                |> Tuple.mapSecond (Cmd.map PageMsg)

        Nothing ->
            ( { model | currentPage = NotFoundPage }, Cmd.none )


{-| Routeに基づいて初期化する
-}
initializePage : Model -> Route -> ( Model, Cmd PageMsg )
initializePage model route =
    case route of
        HomeRoute ->
            ( { model | currentPage = HomePage }, Cmd.none )

        PresetSessionPreparationRoute id ->
            let
                ( sessionModel, cmd ) =
                    SessionPreparationPage.init model.breathingMethods (PresetPracticeStyle id)
            in
            ( { model | currentPage = PresetSessionPreparationPage sessionModel }, Cmd.map PresetSessionPreparationPageMsg cmd )

        ManualSessionPreparationRoute ->
            let
                ( sessionModel, cmd ) =
                    SessionPreparationPage.init model.breathingMethods ManualPracticeStyle
            in
            ( { model | currentPage = ManualSessionPreparationPage sessionModel }, Cmd.map ManualSessionPreparationPageMsg cmd )

        PresetSessionRoute id mduration ->
            let
                ( sessionModel, sessionCmd ) =
                    SessionPage.init model.breathingMethods mduration (SessionPage.PresetBreathingMethod id)
            in
            ( { model | currentPage = PresetSessionPage mduration sessionModel }
            , Cmd.map PresetSessionPageMsg sessionCmd
            )

        ManualSessionRoute mduration minhale minhaleHold mexhale mexhaleHold ->
            let
                ( sessionModel, sessionCmd ) =
                    SessionPage.init
                        model.breathingMethods
                        mduration
                        (SessionPage.CustomBreathingMethod
                            { inhaleDuration = minhale
                            , inhaleHoldDuration = minhaleHold
                            , exhaleDuration = mexhale
                            , exhaleHoldDuration = mexhaleHold
                            }
                        )
            in
            ( { model | currentPage = ManualSessionPage mduration sessionModel }, Cmd.map ManualSessionPageMsg sessionCmd )

        PresetSessionCompletionRoute id mduration ->
            let
                ( completionModel, cmd ) =
                    SessionCompletionPage.init model.breathingMethods mduration (SessionCompletionPage.PresetPracticeStyle id)
            in
            ( { model | currentPage = PresetSessionCompletionPage completionModel }
            , Cmd.map PresetSessionCompletionPageMsg cmd
            )

        ManualSessionCompletionRoute mduration minhale minhaleHold mexhale mexhaleHold ->
            let
                ( completionModel, cmd ) =
                    SessionCompletionPage.init model.breathingMethods mduration (SessionCompletionPage.ManualPracticeStyle minhale minhaleHold mexhale mexhaleHold)
            in
            ( { model | currentPage = ManualSessionCompletionPage completionModel }
            , Cmd.map ManualSessionCompletionPageMsg cmd
            )

        StatisticsRoute ->
            ( { model | currentPage = StatisticsPage }, Cmd.none )

        SettingsRoute ->
            ( { model | currentPage = SettingsPage }, Cmd.none )

        SourceSelectionRoute ->
            let
                ( newModel, cmd ) =
                    SourceSelectionPage.init ()
            in
            ( { model | currentPage = SourceSelectionPage newModel }, Cmd.map SourceSelectionPageMsg cmd )

        BreathingMethodEditRoute id ->
            let
                ( newModel, cmd ) =
                    BreathingMethodPage.init model.breathingMethods (BreathingMethodPage.Edit id)
            in
            ( { model | currentPage = BreathingMethodEditPage newModel }, Cmd.map BreathingMethodEditPageMsg cmd )

        BreathingMethodAddRoute name inhale inhaleHold exhale exhaleHold ->
            let
                ( newModel, cmd ) =
                    BreathingMethodPage.init model.breathingMethods (BreathingMethodPage.Add name inhale inhaleHold exhale exhaleHold)
            in
            ( { model | currentPage = BreathingMethodAddPage newModel }, Cmd.map BreathingMethodAddPageMsg cmd )


{-| ページのメッセージ
-}
type PageMsg
    = PresetSessionPageMsg SessionPage.Msg
    | ManualSessionPageMsg SessionPage.Msg
    | PresetSessionPreparationPageMsg SessionPreparationPage.Msg
    | ManualSessionPreparationPageMsg SessionPreparationPage.Msg
    | PresetSessionCompletionPageMsg SessionCompletionPage.Msg
    | ManualSessionCompletionPageMsg SessionCompletionPage.Msg
    | SourceSelectionPageMsg SourceSelectionPage.Msg
    | BreathingMethodEditPageMsg BreathingMethodPage.Msg
    | BreathingMethodAddPageMsg BreathingMethodPage.Msg


{-| メッセージ
-}
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavigateToRoute Route
      -- Page
    | PageMsg PageMsg
      -- Add these messages
    | ReceiveQueryResult (Result QueryError QueryResult)
    | ReceiveQueryError QueryError
      -- Add saving messages for test
    | UuidMsg (Uuid.Msg Msg)
    | CmdMsg (Cmd Msg)


{-| 画面を更新するためのNoOpのリスト
-}
noOps : List PageMsg
noOps =
    let
        firstMsg =
            PresetSessionPageMsg SessionPage.noOp

        next msg =
            case msg of
                PresetSessionPageMsg _ ->
                    Just (ManualSessionPageMsg SessionPage.noOp)

                ManualSessionPageMsg _ ->
                    Just (PresetSessionPreparationPageMsg SessionPreparationPage.noOp)

                PresetSessionPreparationPageMsg _ ->
                    Just (ManualSessionPreparationPageMsg SessionPreparationPage.noOp)

                ManualSessionPreparationPageMsg _ ->
                    Just (PresetSessionCompletionPageMsg SessionCompletionPage.noOp)

                PresetSessionCompletionPageMsg _ ->
                    Just (ManualSessionCompletionPageMsg SessionCompletionPage.noOp)

                ManualSessionCompletionPageMsg _ ->
                    Just (SourceSelectionPageMsg SourceSelectionPage.noOp)

                SourceSelectionPageMsg _ ->
                    Just (BreathingMethodEditPageMsg BreathingMethodPage.noOp)

                BreathingMethodEditPageMsg _ ->
                    Just (BreathingMethodAddPageMsg BreathingMethodPage.noOp)

                BreathingMethodAddPageMsg _ ->
                    Nothing

        generateList acc =
            case Maybe.andThen next (List.head acc) of
                Just nextMsg ->
                    generateList (nextMsg :: acc)

                Nothing ->
                    acc
    in
    generateList
        [ firstMsg
        ]


{-| リンクがクリックされたときの処理
-}
handleLinkClicked : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
handleLinkClicked urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


{-| 既存セッション中画面のメッセージを処理する

Presetを処理していることに注意する

-}
handleStartSessionPageMsg : SessionPage.Msg -> Model -> ( Model, Cmd Msg )
handleStartSessionPageMsg msg model =
    case model.currentPage of
        PresetSessionPage duration sessionModel ->
            let
                ( newSessionModel, cmd, newRegistry ) =
                    SessionPage.update model.breathingMethods duration model.key msg sessionModel (PageMsg << PresetSessionPageMsg) model.uuidRegistry
            in
            ( { model
                | currentPage = PresetSessionPage duration newSessionModel
                , uuidRegistry = newRegistry
              }
            , cmd
            )

        _ ->
            ( model, Cmd.none )


{-| カスタムセッション中画面のメッセージを処理する

Manualを処理していることに注意する

-}
handleStartCustomSessionPageMsg : SessionPage.Msg -> Model -> ( Model, Cmd Msg )
handleStartCustomSessionPageMsg msg model =
    case model.currentPage of
        ManualSessionPage duration sessionModel ->
            let
                ( newSessionModel, cmd, newRegistry ) =
                    SessionPage.update model.breathingMethods duration model.key msg sessionModel (PageMsg << ManualSessionPageMsg) model.uuidRegistry
            in
            ( { model
                | currentPage = ManualSessionPage duration newSessionModel
                , uuidRegistry = newRegistry
              }
            , cmd
            )

        _ ->
            ( model, Cmd.none )


{-| 既存セッション準備画面のメッセージを処理する

PresetSessionPreparationPageMsgを処理していることに注意する

-}
handlePresetSessionPreparationPageMsg : SessionPreparationPage.Msg -> Model -> ( Model, Cmd Msg )
handlePresetSessionPreparationPageMsg msg model =
    case model.currentPage of
        PresetSessionPreparationPage prepareModel ->
            let
                ( newPrepareModel, cmd ) =
                    SessionPreparationPage.update
                        model.breathingMethods
                        model.key
                        msg
                        prepareModel
            in
            ( { model | currentPage = PresetSessionPreparationPage newPrepareModel }
            , Cmd.map (PageMsg << PresetSessionPreparationPageMsg) cmd
            )

        _ ->
            ( model, Cmd.none )


{-| カスタムセッション準備画面のメッセージを処理する

ManualSessionPreparationPageMsgを処理していることに注意する

-}
handleManualSessionPreparationPageMsg : SessionPreparationPage.Msg -> Model -> ( Model, Cmd Msg )
handleManualSessionPreparationPageMsg msg model =
    case model.currentPage of
        ManualSessionPreparationPage prepareModel ->
            let
                ( newPrepareModel, cmd ) =
                    SessionPreparationPage.update
                        model.breathingMethods
                        model.key
                        msg
                        prepareModel
            in
            ( { model | currentPage = ManualSessionPreparationPage newPrepareModel }
            , Cmd.map (PageMsg << ManualSessionPreparationPageMsg) cmd
            )

        _ ->
            ( model, Cmd.none )


{-| 既存セッション完了画面のメッセージを処理する
-}
handlePresetSessionCompletionPageMsg : SessionCompletionPage.Msg -> Model -> ( Model, Cmd Msg )
handlePresetSessionCompletionPageMsg msg model =
    case model.currentPage of
        PresetSessionCompletionPage completionModel ->
            let
                ( newCompletionModel, cmd ) =
                    SessionCompletionPage.update model.breathingMethods model.key msg completionModel
            in
            ( { model | currentPage = PresetSessionCompletionPage newCompletionModel }
            , Cmd.map (PageMsg << PresetSessionCompletionPageMsg) cmd
            )

        _ ->
            ( model, Cmd.none )


{-| カスタムセッション完了画面のメッセージを処理する
-}
handleManualSessionCompletionPageMsg : SessionCompletionPage.Msg -> Model -> ( Model, Cmd Msg )
handleManualSessionCompletionPageMsg msg model =
    case model.currentPage of
        ManualSessionCompletionPage completionModel ->
            let
                ( newCompletionModel, cmd ) =
                    SessionCompletionPage.update model.breathingMethods model.key msg completionModel
            in
            ( { model | currentPage = ManualSessionCompletionPage newCompletionModel }
            , Cmd.map (PageMsg << ManualSessionCompletionPageMsg) cmd
            )

        _ ->
            ( model, Cmd.none )


{-| ソース選択画面のメッセージを処理する
-}
handleSourceSelectionPageMsg : SourceSelectionPage.Msg -> Model -> ( Model, Cmd Msg )
handleSourceSelectionPageMsg msg model =
    case model.currentPage of
        SourceSelectionPage sourceSelectionModel ->
            let
                ( newSourceSelectionModel, cmd ) =
                    SourceSelectionPage.update model.config model.key msg sourceSelectionModel
            in
            ( { model | currentPage = SourceSelectionPage newSourceSelectionModel }
            , Cmd.map (PageMsg << SourceSelectionPageMsg) cmd
            )

        _ ->
            ( model, Cmd.none )


{-| 呼吸法編集画面のメッセージを処理する
-}
handleBreathingMethodEditPageMsg : BreathingMethodPage.Msg -> Model -> ( Model, Cmd Msg )
handleBreathingMethodEditPageMsg msg model =
    case model.currentPage of
        BreathingMethodEditPage editModel ->
            let
                ( newEditModel, cmd, newRegistry ) =
                    BreathingMethodPage.update model.breathingMethods model.key model.uuidRegistry (PageMsg << BreathingMethodEditPageMsg) msg editModel
            in
            ( { model
                | currentPage = BreathingMethodEditPage newEditModel
                , uuidRegistry = newRegistry
              }
            , cmd
            )

        _ ->
            ( model, Cmd.none )


{-| 呼吸法追加画面のメッセージを処理する
-}
handleBreathingMethodAddPageMsg : BreathingMethodPage.Msg -> Model -> ( Model, Cmd Msg )
handleBreathingMethodAddPageMsg msg model =
    case model.currentPage of
        BreathingMethodAddPage addModel ->
            let
                ( newAddModel, cmd, newRegistry ) =
                    BreathingMethodPage.update model.breathingMethods model.key model.uuidRegistry (PageMsg << BreathingMethodAddPageMsg) msg addModel
            in
            ( { model
                | currentPage = BreathingMethodAddPage newAddModel
                , uuidRegistry = newRegistry
              }
            , cmd
            )

        _ ->
            ( model, Cmd.none )


{-| Queryの結果を処理する
-}
handleReceiveOkQueryResult : QueryResult -> Model -> ( Model, Cmd Msg )
handleReceiveOkQueryResult queryResult model =
    case queryResult of
        QueryResult.CategoryListResult categories ->
            ( { model | categories = Success categories }, Cmd.none )

        QueryResult.BreathingMethodListResult breathingMethods ->
            ( { model | breathingMethods = Success breathingMethods }, Cmd.none )

        QueryResult.SessionListResult sessions ->
            ( { model | sessions = Success sessions }, Cmd.none )

        QueryResult.EntitySingleResult _ ->
            ( model, Cmd.none )


{-| Queryのエラーを処理する
-}
handleReceiveErrQueryResult : QueryError -> Model -> ( Model, Cmd Msg )
handleReceiveErrQueryResult _ model =
    ( model, Cmd.none )


{-| Queryの結果を処理する
-}
handleReceiveQueryResult : Result QueryError QueryResult -> Model -> ( Model, Cmd Msg )
handleReceiveQueryResult result model =
    case result of
        Ok queryResult ->
            handleReceiveOkQueryResult queryResult model

        Err queryError ->
            handleReceiveErrQueryResult queryError model


{-| UUIDのメッセージを処理する
-}
handleUuidMsg : Uuid.Msg Msg -> Model -> ( Model, Cmd Msg )
handleUuidMsg msg model =
    let
        ( newUuidRegistry, effect, maybeMsg ) =
            Uuid.update msg model.uuidRegistry

        cmd =
            Uuid.effectToCmd Ports.generateUuidValue effect

        newModel =
            { model | uuidRegistry = newUuidRegistry }
    in
    case maybeMsg of
        Just m ->
            update m newModel

        Nothing ->
            ( newModel, cmd )


{-| ページのメッセージを処理する
-}
updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case msg of
        PresetSessionPageMsg subMsg ->
            handleStartSessionPageMsg subMsg model

        ManualSessionPageMsg subMsg ->
            handleStartCustomSessionPageMsg subMsg model

        PresetSessionPreparationPageMsg subMsg ->
            handlePresetSessionPreparationPageMsg subMsg model

        ManualSessionPreparationPageMsg subMsg ->
            handleManualSessionPreparationPageMsg subMsg model

        PresetSessionCompletionPageMsg subMsg ->
            handlePresetSessionCompletionPageMsg subMsg model

        ManualSessionCompletionPageMsg subMsg ->
            handleManualSessionCompletionPageMsg subMsg model

        SourceSelectionPageMsg subMsg ->
            handleSourceSelectionPageMsg subMsg model

        BreathingMethodEditPageMsg subMsg ->
            handleBreathingMethodEditPageMsg subMsg model

        BreathingMethodAddPageMsg subMsg ->
            handleBreathingMethodAddPageMsg subMsg model


{-| モデルの更新
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            handleLinkClicked urlRequest model

        UrlChanged url ->
            updateModelFromUrl model url

        NavigateToRoute route ->
            ( model, Nav.pushUrl model.key (Route.toString route) )

        PageMsg pageMsg ->
            updatePage pageMsg model

        ReceiveQueryResult result ->
            handleReceiveQueryResult result
                model
                |> Tuple.mapSecond
                    (\c ->
                        Cmd.batch
                            [ c
                            , noOps
                                |> List.map (PageMsg >> always >> Task.perform >> (|>) Time.now)
                                |> Cmd.batch
                                |> CmdMsg
                                |> always
                                |> Task.perform
                                |> (|>) Time.now
                            ]
                    )

        ReceiveQueryError error ->
            handleReceiveErrQueryResult error model

        UuidMsg uuidMsg ->
            handleUuidMsg uuidMsg model

        CmdMsg cmd ->
            ( model, cmd )


{-| ビュー
-}
view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.currentPage
    , body =
        [ viewPage model
        ]
    }


{-| ページタイトルを返す関数
-}
pageTitle : Page -> String
pageTitle page =
    case page of
        HomePage ->
            "呼吸法アプリ"

        PresetSessionPreparationPage _ ->
            "準備画面"

        ManualSessionPreparationPage _ ->
            "カスタム準備画面"

        PresetSessionPage _ _ ->
            "セッション開始"

        ManualSessionPage _ _ ->
            "カスタムセッション開始"

        PresetSessionCompletionPage _ ->
            "セッション完了"

        ManualSessionCompletionPage _ ->
            "カスタムセッション完了"

        StatisticsPage ->
            "統計"

        SettingsPage ->
            "設定"

        SourceSelectionPage _ ->
            "ソース選択"

        BreathingMethodEditPage _ ->
            "呼吸法編集"

        BreathingMethodAddPage _ ->
            "呼吸法追加"

        NotFoundPage ->
            "ページが見つかりません"


{-| ページの基本となるビュー
-}
viewPage : Model -> Html Msg
viewPage model =
    div
        [ class "h-screen flex flex-col"
        ]
    <|
        viewContent { viewNav = viewNav, viewFooter = viewFooter } model


{-| ストリークのビュー
-}
viewStreak : Int -> Html msg
viewStreak streak =
    div [ class "flex items-center space-x-2" ]
        [ Icon.view Icon.Flame
        , span
            [ class "font-medium text-xs"
            ]
            [ text <| String.fromInt streak
            ]
        ]


{-| ナビゲーションのビュー
-}
viewNav : Html Msg
viewNav =
    nav [ class "bg-white shadow-sm px-4 py-3 flex justify-end items-center space-x-4" ]
        [ viewStreak 30
        , button
            [ attribute "aria-label" "settings"
            , class "p-2 hover:bg-gray-200 rounded-full"
            , onClick (NavigateToRoute SettingsRoute)
            ]
            [ Icon.view Icon.Settings ]
        ]


{-| フッターのビュー
-}
viewFooter : Html Msg
viewFooter =
    let
        tabClass =
            class "flex flex-col items-center space-y-1"
    in
    footer
        [ class "bg-white shadow-lg px-4 py-3"
        ]
        [ div [ class "max-w-2xl mx-auto flex justify-around" ]
            [ button
                [ attribute "aria-label" "home-tab"
                , onClick (NavigateToRoute HomeRoute)
                , tabClass
                ]
                [ Icon.view Icon.Home
                , span [ class "text-sm" ] [ text "ホーム" ]
                ]
            , button
                [ attribute "aria-label" "start-session-prepare"
                , onClick (NavigateToRoute ManualSessionPreparationRoute)
                , tabClass
                ]
                [ Icon.view Icon.Play
                , span [ class "text-sm" ] [ text "セッション開始" ]
                ]
            , button
                [ attribute "aria-label" "statistics-tab"
                , onClick (NavigateToRoute StatisticsRoute)
                , tabClass
                ]
                [ Icon.view Icon.Statistics
                , span [ class "text-sm" ] [ text "統計" ]
                ]
            ]
        ]


{-| 呼吸法カードのビュー
-}
viewBreathingMethodCard : BreathingMethod -> Html Msg
viewBreathingMethodCard breathingMethod =
    article
        [ attribute "aria-label" "breathing-method-card"
        , attribute "data-id" <| Uuid.toString breathingMethod.id
        , onClick
            (NavigateToRoute <|
                PresetSessionPreparationRoute breathingMethod.id
            )
        , class "p-4 bg-white rounded-lg shadow hover:shadow-md transition-shadow"
        ]
        [ h3
            [ class "text-lg font-medium"
            ]
            [ text <| Types.BreathingMethod.fromName breathingMethod.name
            ]
        ]


{-| 呼吸法リストのビュー
-}
viewBreathingMethodList : Category -> List (Html Msg) -> Html Msg
viewBreathingMethodList category children =
    section
        [ attribute "aria-label" "category"
        , attribute "data-id" <| Uuid.toString category.id
        , class "mb-6"
        ]
        [ h2
            [ attribute "role" "category-title"
            , class "text-xl font-bold mb-3"
            ]
            [ text <| fromTitle category.title ]
        , ul
            [ class "space-y-3" ]
            children
        ]


{-| ホーム画面のビュー
-}
viewHome : { model | categories : RemoteData e (List Category), breathingMethods : RemoteData e (List BreathingMethod) } -> View Msg
viewHome model =
    { nav = True
    , footer = True
    , view =
        case ( model.categories, model.breathingMethods ) of
            ( Success cs, Success ms ) ->
                div
                    [ attribute "role" "home"
                    , class "max-w-2xl mx-auto space-y-6"
                    ]
                    [ div [] <|
                        List.map
                            (\category ->
                                viewBreathingMethodList
                                    category
                                <|
                                    List.filterMap
                                        (\method ->
                                            if method.categoryId == category.id then
                                                Just (viewBreathingMethodCard method)

                                            else
                                                Nothing
                                        )
                                        ms
                            )
                            cs
                    , button
                        [ attribute "aria-label" "add-new-breathing-method"
                        , onClick (NavigateToRoute SourceSelectionRoute)
                        , class "w-full py-3 px-4 bg-blue-500 text-white rounded-lg flex items-center justify-center space-x-2 hover:bg-blue-600 transition-colors"
                        ]
                        [ Icon.view Icon.Plus
                        , text "新しい呼吸法を追加"
                        ]
                    ]

            ( _, _ ) ->
                text "Loading or failure..."
    }


{-| 統計画面のビュー
-}
viewStatistics : Model -> Html Msg
viewStatistics model =
    case model.sessions of
        NotAsked ->
            text "NotAsked"

        Loading ->
            text "Loading..."

        Failure _ ->
            text "Failure"

        Success sessions ->
            let
                statistics =
                    calculateFromSessions sessions

                recentStatistics =
                    calculateRecentFromSessions recentDaysThreshold model.now sessions
            in
            div [ attribute "role" "statistics" ]
                [ div [ attribute "aria-label" "streak-display" ] [ text "" ]
                , section
                    [ attribute "aria-label" "recent-7-days"
                    , class "p-4 rounded-lg shadow"
                    ]
                    [ h2 [ class "text-lg font-semibold mb-4" ] [ text "過去7日間" ]
                    , div [ class "grid grid-cols-2 gap-4" ]
                        [ div [ class "flex items-center gap-3" ]
                            [ span [ class "text-2xl" ] [ text "🎯" ]
                            , div [ class "grid grid-flow-col gap-1 items-baseline" ]
                                [ span
                                    [ attribute "aria-label" "recent-sets"
                                    , class "text-2xl font-bold"
                                    ]
                                    [ text <| String.fromInt recentStatistics.totalSets ]
                                , span [ class "text-sm text-gray-500" ] [ text "セット数" ]
                                ]
                            ]
                        , div [ class "flex items-center gap-3" ]
                            [ span [ class "text-2xl" ] [ text "⏱️" ]
                            , div [ class "grid grid-flow-col gap-1 items-baseline" ]
                                [ span
                                    [ attribute "aria-label" "recent-minutes"
                                    , class "text-2xl font-bold"
                                    ]
                                    [ text <| String.fromInt <| floor <| (\s -> s / 60) <| toFloat recentStatistics.totalSeconds ]
                                , span [ class "text-sm text-gray-500" ] [ text "練習時間(分)" ]
                                ]
                            ]
                        ]
                    ]
                , section
                    [ attribute "aria-label" "total"
                    , class "p-4 rounded-lg shadow mt-4"
                    ]
                    [ h2 [ class "text-lg font-semibold mb-4" ] [ text "累計" ]
                    , div [ class "grid grid-cols-2 gap-4" ]
                        [ div [ class "flex items-center gap-3" ]
                            [ span [ class "text-2xl" ] [ text "📊" ]
                            , div [ class "grid grid-flow-col gap-1 items-baseline" ]
                                [ span
                                    [ attribute "aria-label" "total-sets"
                                    , class "text-2xl font-bold"
                                    ]
                                    [ text <| String.fromInt statistics.totalSets ]
                                , span [ class "text-sm text-gray-500" ] [ text "総セット数" ]
                                ]
                            ]
                        , div [ class "flex items-center gap-3" ]
                            [ span [ class "text-2xl" ] [ text "⏱️" ]
                            , div [ class "grid grid-flow-col gap-1 items-baseline" ]
                                [ span
                                    [ attribute "aria-label" "total-minutes"
                                    , class "text-2xl font-bold"
                                    ]
                                    [ text <| String.fromInt <| floor <| (\s -> s / 60) <| toFloat statistics.totalSeconds ]
                                , span [ class "text-sm text-gray-500" ] [ text "総練習時間(秒)" ]
                                ]
                            ]
                        ]
                    ]
                , section
                    [ attribute "aria-label" "practice-days"
                    , class "p-4 rounded-lg shadow mt-4"
                    ]
                    [ h2 [ class "text-lg font-semibold mb-4" ] [ text "練習記録" ]
                    , div [ class "grid grid-cols-2 gap-4" ]
                        [ div [ class "flex items-center gap-3" ]
                            [ span [ class "text-2xl" ] [ text "📅" ]
                            , div [ class "grid grid-flow-col gap-1 items-baseline" ]
                                [ span
                                    [ attribute "aria-label" "total-practice-days"
                                    , class "text-2xl font-bold"
                                    ]
                                    [ text <| String.fromInt statistics.totalPracticeDays ]
                                , span [ class "text-sm text-gray-500" ] [ text "練習日数" ]
                                ]
                            ]
                        ]
                    ]
                ]


{-| 設定画面のビュー
-}
viewSettings : Html Msg
viewSettings =
    div [ attribute "role" "settings" ] [ text "設定画面" ]


{-| ページが見つからなかった場合のビュー
-}
viewNotFound : Html msg
viewNotFound =
    div [] [ text "404 - ページが見つかりません" ]


{-| ページに応じたビューを返す関数。
-}
viewContent : { viewNav : Html Msg, viewFooter : Html Msg } -> Model -> List (Html Msg)
viewContent views model =
    (\opt ->
        List.filterMap identity
            [ Maybe.Extra.filter (always opt.nav)
                (Just views.viewNav)
            , Just
                (main_
                    [ class "flex-1 px-4 py-6 overflow-scroll"
                    ]
                    [ opt.view
                    ]
                )
            , Maybe.Extra.filter (always opt.footer)
                (Just views.viewFooter)
            ]
    )
    <|
        case model.currentPage of
            HomePage ->
                viewHome model

            PresetSessionPreparationPage prepareModel ->
                SessionPreparationPage.view prepareModel
                    |> View.map (PresetSessionPreparationPageMsg >> PageMsg)

            ManualSessionPreparationPage prepareModel ->
                SessionPreparationPage.view prepareModel
                    |> View.map (ManualSessionPreparationPageMsg >> PageMsg)

            PresetSessionPage duration sessionModel ->
                SessionPage.view duration sessionModel
                    |> View.map (PresetSessionPageMsg >> PageMsg)

            ManualSessionPage duration sessionModel ->
                SessionPage.view duration sessionModel
                    |> View.map (ManualSessionPageMsg >> PageMsg)

            PresetSessionCompletionPage completionModel ->
                SessionCompletionPage.view completionModel
                    |> View.map (PresetSessionCompletionPageMsg >> PageMsg)

            ManualSessionCompletionPage completionModel ->
                SessionCompletionPage.view completionModel
                    |> View.map (ManualSessionCompletionPageMsg >> PageMsg)

            StatisticsPage ->
                { view = viewStatistics model
                , nav = True
                , footer = True
                }

            SettingsPage ->
                { nav = False
                , footer = False
                , view = viewSettings
                }

            SourceSelectionPage sourceSelectionModel ->
                SourceSelectionPage.view sourceSelectionModel
                    |> View.map (SourceSelectionPageMsg >> PageMsg)

            BreathingMethodEditPage editModel ->
                BreathingMethodPage.view model.categories editModel
                    |> View.map (BreathingMethodEditPageMsg >> PageMsg)

            BreathingMethodAddPage addModel ->
                BreathingMethodPage.view model.categories addModel
                    |> View.map (BreathingMethodAddPageMsg >> PageMsg)

            NotFoundPage ->
                { view = viewNotFound
                , nav = False
                , footer = False
                }


{-| ホーム画面のサブスクリプション
-}
homeSubscriptions : Sub Msg
homeSubscriptions =
    Sub.none


{-| 既存セッション準備画面のサブスクリプション
-}
presetSessionPreparationSubscriptions : Sub Msg
presetSessionPreparationSubscriptions =
    Sub.none


{-| カスタムセッション準備画面のサブスクリプション
-}
manualSessionPreparationSubscriptions : Sub Msg
manualSessionPreparationSubscriptions =
    Sub.none


{-| セッション中画面のサブスクリプション
-}
presetSessionSubscriptions : SessionPage.Model -> Sub Msg
presetSessionSubscriptions sessionModel =
    SessionPage.subscriptions sessionModel
        |> Sub.map (PresetSessionPageMsg >> PageMsg)


{-| カスタムセッション中画面のサブスクリプション
-}
manualSessionSubscriptions : SessionPage.Model -> Sub Msg
manualSessionSubscriptions sessionModel =
    SessionPage.subscriptions sessionModel
        |> Sub.map (ManualSessionPageMsg >> PageMsg)


{-| セッション完了画面のサブスクリプション
-}
presetSessionCompletionSubscriptions : Sub Msg
presetSessionCompletionSubscriptions =
    Sub.none


{-| カスタムセッション完了画面のサブスクリプション
-}
manualSessionCompletionSubscriptions : Sub Msg
manualSessionCompletionSubscriptions =
    Sub.none


{-| 統計画面のサブスクリプション
-}
statisticsSubscriptions : Sub Msg
statisticsSubscriptions =
    Sub.none


{-| 設定画面のサブスクリプション
-}
settingsSubscriptions : Sub Msg
settingsSubscriptions =
    Sub.none


{-| ソース選択画面のサブスクリプション
-}
sourceSelectionSubscriptions : Sub Msg
sourceSelectionSubscriptions =
    Sub.none


{-| 呼吸法編集画面のサブスクリプション
-}
breathingMethodEditSubscriptions : Sub Msg
breathingMethodEditSubscriptions =
    Sub.none


{-| 呼吸法追加画面のサブスクリプション
-}
breathingMethodAddSubscriptions : Sub Msg
breathingMethodAddSubscriptions =
    Sub.none


{-| ページに応じたサブスクリプションを返す関数。
-}
pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        HomePage ->
            homeSubscriptions

        PresetSessionPreparationPage _ ->
            presetSessionPreparationSubscriptions

        ManualSessionPreparationPage _ ->
            manualSessionPreparationSubscriptions

        PresetSessionPage _ sessionModel ->
            presetSessionSubscriptions sessionModel

        ManualSessionPage _ sessionModel ->
            manualSessionSubscriptions sessionModel

        PresetSessionCompletionPage _ ->
            presetSessionCompletionSubscriptions

        ManualSessionCompletionPage _ ->
            manualSessionCompletionSubscriptions

        StatisticsPage ->
            statisticsSubscriptions

        SettingsPage ->
            settingsSubscriptions

        SourceSelectionPage _ ->
            sourceSelectionSubscriptions

        BreathingMethodEditPage _ ->
            breathingMethodEditSubscriptions

        BreathingMethodAddPage _ ->
            breathingMethodAddSubscriptions

        NotFoundPage ->
            Sub.none


{-| サブスクリプション
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model.currentPage
        , Ports.subscribeToQueryResults ReceiveQueryResult
        , Ports.subscribeToQueryErrors ReceiveQueryError
        , Uuid.subscriptions Ports.subscribeToUuid UuidMsg
        ]


{-| Entrypoint
-}
main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
