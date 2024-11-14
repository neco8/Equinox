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
import Pages.BreathingMethodPage as BreathingMethodPage
import Pages.SessionCompletionPage as SessionCompletionPage
import Pages.SessionPage as SessionPage exposing (subscriptions, view)
import Pages.SessionPreparationPage as SessionPreparationPage exposing (PracticeStyle(..))
import Pages.SourceSelectionPage as SourceSelectionPage
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, PhaseType(..))
import Types.Category exposing (Category, fromTitle)
import Types.Session exposing (Duration, Session)
import Types.Statistics exposing (recentDaysThreshold)
import Url
import Uuid


{-| Model
-}
type alias Model =
    { key : Nav.Key
    , config : Config
    , currentPage : Page
    , uuidRegistry : Uuid.Registry Msg
    , uuids : List Uuid.Uuid
    , breathingMethods : RemoteData () (List BreathingMethod) -- 暫定的にerrorは()でおく
    , categories : RemoteData () (List Category) -- 暫定的にerrorは()でおく
    , recentSessions : RemoteData () (List Session) -- 暫定的にerrorは()でおく
    }


{-| ページの種類
-}
type Page
    = HomePage
    | PresetSessionPreparationPage BreathingMethodId SessionPreparationPage.Model
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
            , config = Config.config flags.environment
            , currentPage = NotFoundPage
            , uuidRegistry = Uuid.initialRegistry
            , uuids = []
            , breathingMethods = NotAsked
            , categories = NotAsked
            , recentSessions = NotAsked
            }

        initialQueries =
            [ Query.GetAllBreathingMethods
            , Query.GetAllCategories
            , Query.GetSessionRecentNDays recentDaysThreshold flags.now
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

        Nothing ->
            ( { model | currentPage = NotFoundPage }, Cmd.none )


{-| Routeに基づいて初期化する
-}
initializePage : Model -> Route -> ( Model, Cmd Msg )
initializePage model route =
    case route of
        HomeRoute ->
            ( { model | currentPage = HomePage }, Cmd.none )

        PresetSessionPreparationRoute id ->
            let
                ( sessionModel, cmd ) =
                    SessionPreparationPage.init model.breathingMethods (PresetPracticeStyle id)
            in
            ( { model | currentPage = PresetSessionPreparationPage id sessionModel }, Cmd.map PresetSessionPreparationPageMsg cmd )

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
                    SessionCompletionPage.init mduration (PresetPracticeStyle id)
            in
            ( { model | currentPage = PresetSessionCompletionPage completionModel }
            , Cmd.map PresetSessionCompletionPageMsg cmd
            )

        ManualSessionCompletionRoute mduration ->
            let
                ( completionModel, cmd ) =
                    SessionCompletionPage.init mduration ManualPracticeStyle
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


{-| メッセージ
-}
type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavigateToRoute Route
      -- Page
    | PresetSessionPageMsg SessionPage.Msg
    | ManualSessionPageMsg SessionPage.Msg
    | PresetSessionPreparationPageMsg SessionPreparationPage.Msg
    | ManualSessionPreparationPageMsg SessionPreparationPage.Msg
    | PresetSessionCompletionPageMsg SessionCompletionPage.Msg
    | ManualSessionCompletionPageMsg SessionCompletionPage.Msg
    | SourceSelectionPageMsg SourceSelectionPage.Msg
    | BreathingMethodEditPageMsg BreathingMethodPage.Msg
    | BreathingMethodAddPageMsg BreathingMethodPage.Msg
      -- Add these messages
    | ReceiveQueryResult (Result QueryError QueryResult)
    | ReceiveQueryError QueryError
      -- Add saving messages for test
    | UuidMsg (Uuid.Msg Msg)
    | GotUuid Uuid.Uuid


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
                    SessionPage.update model.breathingMethods duration model.key msg sessionModel PresetSessionPageMsg model.uuidRegistry
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
                    SessionPage.update model.breathingMethods duration model.key msg sessionModel ManualSessionPageMsg model.uuidRegistry
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
        PresetSessionPreparationPage id prepareModel ->
            let
                ( newPrepareModel, cmd ) =
                    SessionPreparationPage.update
                        model.breathingMethods
                        model.key
                        msg
                        prepareModel
            in
            ( { model | currentPage = PresetSessionPreparationPage id newPrepareModel }
            , Cmd.map PresetSessionPreparationPageMsg cmd
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
            , Cmd.map ManualSessionPreparationPageMsg cmd
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
                    SessionCompletionPage.update model.key msg completionModel
            in
            ( { model | currentPage = PresetSessionCompletionPage newCompletionModel }
            , Cmd.map PresetSessionCompletionPageMsg cmd
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
                    SessionCompletionPage.update model.key msg completionModel
            in
            ( { model | currentPage = ManualSessionCompletionPage newCompletionModel }
            , Cmd.map ManualSessionCompletionPageMsg cmd
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
            , Cmd.map SourceSelectionPageMsg cmd
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
                    BreathingMethodPage.update model.breathingMethods model.key model.uuidRegistry BreathingMethodEditPageMsg msg editModel
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
                    BreathingMethodPage.update model.breathingMethods model.key model.uuidRegistry BreathingMethodAddPageMsg msg addModel
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
            ( { model | recentSessions = Success sessions }, Cmd.none )

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

        ReceiveQueryResult result ->
            handleReceiveQueryResult result model

        ReceiveQueryError error ->
            handleReceiveErrQueryResult error model

        UuidMsg uuidMsg ->
            handleUuidMsg uuidMsg model

        GotUuid uuid ->
            ( { model
                | uuids =
                    model.uuids ++ [ uuid ]
              }
            , Cmd.none
            )


{-| ビュー
-}
view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.currentPage
    , body =
        [ viewPage model
        , Html.map UuidMsg <| button [ onClick (Uuid.uuidGenerate "main" GotUuid) ] [ text "Generate UUID" ]
        , ul [] <| List.map (\uuid -> li [] [ text <| Uuid.toString uuid ]) model.uuids
        ]
    }


{-| ページタイトルを返す関数
-}
pageTitle : Page -> String
pageTitle page =
    case page of
        HomePage ->
            "呼吸法アプリ"

        PresetSessionPreparationPage _ _ ->
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
    div []
        [ nav [] [ viewNav ]
        , main_ [] [ viewContent model ]
        , viewFooter
        ]


{-| ストリークのビュー
-}
viewStreak : Int -> Html msg
viewStreak streak =
    div []
        [ Icon.view Icon.Flame
        , text <| String.fromInt streak
        ]


{-| ナビゲーションのビュー
-}
viewNav : Html Msg
viewNav =
    ul []
        [ viewStreak 30
        , button [ attribute "aria-label" "settings" ] [ text "設定" ]
        ]


{-| フッターのビュー
-}
viewFooter : Html Msg
viewFooter =
    footer []
        [ button
            [ attribute "aria-label" "home-tab"
            , onClick (NavigateToRoute HomeRoute)
            ]
            [ text "ホーム" ]
        , button
            [ attribute "aria-label" "start-session-prepare"
            , onClick (NavigateToRoute ManualSessionPreparationRoute)
            ]
            [ text "セッション開始" ]
        , button
            [ attribute "aria-label" "statistics-tab"
            , onClick (NavigateToRoute StatisticsRoute)
            ]
            [ text "統計" ]
        ]


{-| 呼吸法カードのビュー
-}
viewBreathingMethodCard : BreathingMethod -> Html Msg
viewBreathingMethodCard breathingMethod =
    article
        [ attribute "aria-label" <| Uuid.toString breathingMethod.id
        , onClick
            (NavigateToRoute <|
                PresetSessionPreparationRoute breathingMethod.id
            )
        ]
        [ text <| Types.BreathingMethod.fromName breathingMethod.name ]


{-| 呼吸法リストのビュー
-}
viewBreathingMethodList : Category -> List (Html Msg) -> Html Msg
viewBreathingMethodList category children =
    ul
        [ attribute "aria-label" <| Uuid.toString category.id ]
    <|
        span [ attribute "role" "category-title" ] [ text <| fromTitle category.title ]
            :: children


{-| ホーム画面のビュー
-}
viewHome : { model | categories : RemoteData e (List Category), breathingMethods : RemoteData e (List BreathingMethod) } -> Html Msg
viewHome model =
    case ( model.categories, model.breathingMethods ) of
        ( Success cs, Success ms ) ->
            div [ attribute "role" "home" ]
                [ text "ホーム画面"
                , div [] <|
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
                    ]
                    [ text "新しい呼吸法を追加" ]
                ]

        ( _, _ ) ->
            text "Loading or failure..."


{-| 既存セッション準備画面のビュー
-}
viewPresetSessionPreparation : SessionPreparationPage.Model -> BreathingMethodId -> Html SessionPreparationPage.Msg
viewPresetSessionPreparation model id =
    let
        txt =
            "準備画面 - ID: " ++ Uuid.toString id
    in
    SessionPreparationPage.view
        { txt = txt
        }
        model


{-| カスタムセッション準備画面のビュー
-}
viewManualSessionPreparation : SessionPreparationPage.Model -> Html SessionPreparationPage.Msg
viewManualSessionPreparation model =
    let
        txt =
            "カスタム準備画面"
    in
    SessionPreparationPage.view
        { txt = txt
        }
        model


{-| 既存セッション完了画面のビュー
-}
viewPresetSessionCompletion : SessionCompletionPage.Model -> Html Msg
viewPresetSessionCompletion model =
    SessionCompletionPage.view model
        |> Html.map PresetSessionCompletionPageMsg


{-| カスタムセッション完了画面のビュー
-}
viewManualSessionCompletion : SessionCompletionPage.Model -> Html Msg
viewManualSessionCompletion model =
    SessionCompletionPage.view model
        |> Html.map ManualSessionCompletionPageMsg


{-| 統計画面のビュー
-}
viewStatistics : Html Msg
viewStatistics =
    div [ attribute "role" "statistics" ]
        [ text "統計画面"
        , button
            [ attribute "aria-label" "home"
            , onClick (NavigateToRoute HomeRoute)
            ]
            [ text "ホーム" ]
        , div [ attribute "aria-label" "streak-display" ] [ text "この中でアニメーションなどが表示されます。" ]
        , section [ attribute "aria-label" "recent-7-days" ]
            [ span [ attribute "aria-label" "recent-sets" ] [ text "10 セット" ]
            , span [ attribute "aria-label" "recent-minutes" ] [ text "30 分" ]
            ]
        , section [ attribute "aria-label" "total" ]
            [ span [ attribute "aria-label" "total-sets" ] [ text "100 セット" ]
            , span [ attribute "aria-label" "total-minutes" ] [ text "300 分" ]
            ]
        , section [ attribute "aria-label" "practice-days" ]
            [ span [] []
            , span [ attribute "aria-label" "total-practice-days" ] [ text "30 日" ]
            ]
        ]


{-| 設定画面のビュー
-}
viewSettings : Html Msg
viewSettings =
    div [ attribute "role" "settings" ] [ text "設定画面" ]


{-| ソース選択画面のビュー
-}
viewSourceSelection : SourceSelectionPage.Model -> Html Msg
viewSourceSelection model =
    SourceSelectionPage.view model
        |> Html.map SourceSelectionPageMsg


{-| 呼吸法編集画面のビュー
-}
viewBreathingMethodEdit : RemoteData e (List Category) -> BreathingMethodPage.Model -> Html Msg
viewBreathingMethodEdit categories model =
    BreathingMethodPage.view categories model
        |> Html.map BreathingMethodEditPageMsg


{-| 呼吸法追加画面のビュー
-}
viewBreathingMethodAdd : RemoteData e (List Category) -> BreathingMethodPage.Model -> Html Msg
viewBreathingMethodAdd categories model =
    BreathingMethodPage.view categories model
        |> Html.map BreathingMethodAddPageMsg


{-| ページが見つからなかった場合のビュー
-}
viewNotFound : Html msg
viewNotFound =
    div [] [ text "404 - ページが見つかりません" ]


{-| ページに応じたビューを返す関数。
-}
viewContent : Model -> Html Msg
viewContent model =
    case model.currentPage of
        HomePage ->
            viewHome model

        PresetSessionPreparationPage id prepareModel ->
            viewPresetSessionPreparation prepareModel id
                |> Html.map PresetSessionPreparationPageMsg

        ManualSessionPreparationPage prepareModel ->
            viewManualSessionPreparation prepareModel
                |> Html.map ManualSessionPreparationPageMsg

        PresetSessionPage duration sessionModel ->
            SessionPage.view duration sessionModel
                |> Html.map PresetSessionPageMsg

        ManualSessionPage duration sessionModel ->
            SessionPage.view duration sessionModel
                |> Html.map ManualSessionPageMsg

        PresetSessionCompletionPage completionModel ->
            viewPresetSessionCompletion completionModel

        ManualSessionCompletionPage completionModel ->
            viewManualSessionCompletion completionModel

        StatisticsPage ->
            viewStatistics

        SettingsPage ->
            viewSettings

        SourceSelectionPage sourceSelectionModel ->
            viewSourceSelection sourceSelectionModel

        BreathingMethodEditPage editModel ->
            viewBreathingMethodEdit model.categories editModel

        BreathingMethodAddPage addModel ->
            viewBreathingMethodAdd model.categories addModel

        NotFoundPage ->
            viewNotFound


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
        |> Sub.map PresetSessionPageMsg


{-| カスタムセッション中画面のサブスクリプション
-}
manualSessionSubscriptions : SessionPage.Model -> Sub Msg
manualSessionSubscriptions sessionModel =
    SessionPage.subscriptions sessionModel
        |> Sub.map ManualSessionPageMsg


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

        PresetSessionPreparationPage _ _ ->
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
