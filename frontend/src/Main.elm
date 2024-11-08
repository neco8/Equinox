module Main exposing (main)

{-|


## Main モジュール

このモジュールはEntrypointです。複数のページをまとめ、中央のModelの管理も行います。


### TODO

  - [ ] TODO: 初期表示にて、アニメーションローディング画面を追加する
  - [ ] TODO: 数値入力input達について、validationとdisabledを管理する
  - [ ] TODO: セッション関係のものはSessionPage.elmへと移動する
  - [ ] TODO: ReceiveQueryResultなど、Queryを送る、受け取る関係のものはより愚直な形へリファクタリングする
  - [ ] TODO: duration系統を、すべて秒単位であることがわかりやすいように、claudeの仕様・e2e・frontendをすべて修正する
  - [ ] TODO: 後で、PrepareSessionPageはページに移動することになる
  - [ ] TODO: Streakを計算する関数を追加する

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon
import JS.Ports as Ports
import JS.Storage.QueryError exposing (QueryError)
import JS.Storage.QueryResult as QueryResult exposing (QueryResult)
import JS.Storage.StorageQueryDSL as Query
import List.Extra
import Maybe.Extra
import Pages.SessionCompletionPage as SessionCompletionPage
import Pages.SessionPage as SessionPage exposing (subscriptions, view)
import Pages.SessionPreparationPage as SessionPreparationPage exposing (PracticeStyle(..))
import Route exposing (Route(..))
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, PhaseType(..))
import Types.Category exposing (Category)
import Types.Session exposing (Duration, Session)
import Types.Statistics exposing (recentDaysThreshold)
import Url
import Uuid


{-| Model
-}
type alias Model =
    { key : Nav.Key
    , currentPage : Page
    , uuidGenerator : Uuid.Generator Msg
    , uuids : List Uuid.Uuid
    , breathingMethods : List BreathingMethod
    , categories : List Category
    , recentSessions : List Session
    }


{-| ページの種類
-}
type Page
    = HomePage
    | PresetSessionPreparationPage BreathingMethodId SessionPreparationPage.Model
    | ManualSessionPreparationPage SessionPreparationPage.Model
    | PresetSessionPage Duration SessionPage.Model
    | ManualSessionPage Duration SessionPage.Model
    | PresetSessionCompletionPage Duration BreathingMethodId
    | ManualSessionCompletionPage Duration
    | StatisticsPage
    | SettingsPage
    | SourceSelectionPage
    | BreathingMethodEditPage BreathingMethodId
    | NotFoundPage


{-| Modelを初期化する
-}
init : Int -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init now url key =
    let
        model : Model
        model =
            { key = key
            , currentPage = NotFoundPage
            , uuidGenerator = Uuid.initialModel
            , uuids = []
            , breathingMethods = []
            , categories = []
            , recentSessions = []
            }

        initialQueries =
            [ Query.GetAllBreathingMethods
            , Query.GetAllCategories
            , Query.GetSessionRecentNDays recentDaysThreshold (Time.millisToPosix now)
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


{-| 呼吸法の各フェーズの時間を解析する

Routeのパラメータから、各フェーズの時間を解析する

-}
parseBreathingMethodControls :
    { inhale : Maybe InhaleDuration
    , inhaleHold : Maybe InhaleHoldDuration
    , exhale : Maybe ExhaleDuration
    , exhaleHold : Maybe ExhaleHoldDuration
    }
    ->
        Maybe
            { inhaleDuration : InhaleDuration
            , inhaleHoldDuration : InhaleHoldDuration
            , exhaleDuration : ExhaleDuration
            , exhaleHoldDuration : ExhaleHoldDuration
            }
parseBreathingMethodControls { inhale, inhaleHold, exhale, exhaleHold } =
    case ( ( inhale, inhaleHold ), ( exhale, exhaleHold ) ) of
        ( ( Just i, Just ih ), ( Just e, Just eh ) ) ->
            Just
                { inhaleDuration = i
                , inhaleHoldDuration = ih
                , exhaleDuration = e
                , exhaleHoldDuration = eh
                }

        _ ->
            Nothing


{-| Routeに基づいて初期化する
-}
initializePage : Model -> Route -> ( Model, Cmd Msg )
initializePage model route =
    case route of
        HomeRoute ->
            ( { model | currentPage = HomePage }, Cmd.none )

        PresetSessionPreparationRoute id ->
            ( { model | currentPage = PresetSessionPreparationPage id (SessionPreparationPage.init ()) }, Cmd.none )

        ManualSessionPreparationRoute ->
            ( { model | currentPage = ManualSessionPreparationPage (SessionPreparationPage.init ()) }, Cmd.none )

        PresetSessionRoute id mduration ->
            case
                ( List.Extra.find (\m -> m.id == id) model.breathingMethods
                , mduration
                )
            of
                ( Just method, Just duration ) ->
                    let
                        ( sessionModel, sessionCmd ) =
                            SessionPage.init (SessionPage.Existing method)
                    in
                    ( { model | currentPage = PresetSessionPage duration sessionModel }
                    , Cmd.map PresetSessionPageMsg sessionCmd
                    )

                ( Nothing, _ ) ->
                    -- 呼吸法が見つからない場合、NotFoundPageにリダイレクトする
                    ( { model | currentPage = NotFoundPage }, Cmd.none )

                ( Just _, Nothing ) ->
                    -- セッション秒数が設定されていない場合、準備画面にリダイレクトし入力を促す
                    ( model, Nav.replaceUrl model.key (Route.toString (Route.PresetSessionPreparationRoute id)) )

        ManualSessionRoute mduration minhale minhaleHold mexhale mexhaleHold ->
            case
                Just Tuple.pair
                    |> Maybe.Extra.andMap (parseBreathingMethodControls { inhale = minhale, inhaleHold = minhaleHold, exhale = mexhale, exhaleHold = mexhaleHold })
                    |> Maybe.Extra.andMap mduration
            of
                Just ( { inhaleDuration, inhaleHoldDuration, exhaleDuration, exhaleHoldDuration }, duration ) ->
                    let
                        ( sessionModel, sessionCmd ) =
                            SessionPage.init
                                (SessionPage.Custom
                                    { inhaleDuration = inhaleDuration
                                    , inhaleHoldDuration = inhaleHoldDuration
                                    , exhaleDuration = exhaleDuration
                                    , exhaleHoldDuration = exhaleHoldDuration
                                    }
                                )
                    in
                    ( { model | currentPage = ManualSessionPage duration sessionModel }, Cmd.map ManualSessionPageMsg sessionCmd )

                Nothing ->
                    -- 呼吸法・セッション秒数が設定されていない場合、準備画面にリダイレクトし入力を促す
                    ( model, Nav.replaceUrl model.key (Route.toString ManualSessionPreparationRoute) )

        PresetSessionCompletionRoute id mduration ->
            case mduration of
                Just duration ->
                    ( { model | currentPage = PresetSessionCompletionPage duration id }, Cmd.none )

                Nothing ->
                    ( model
                    , Nav.replaceUrl model.key (Route.toString (PresetSessionPreparationRoute id))
                    )

        ManualSessionCompletionRoute mduration ->
            case mduration of
                Just duration ->
                    ( { model | currentPage = ManualSessionCompletionPage duration }, Cmd.none )

                Nothing ->
                    ( model, Nav.replaceUrl model.key (Route.toString ManualSessionPreparationRoute) )

        StatisticsRoute ->
            ( { model | currentPage = StatisticsPage }, Cmd.none )

        SettingsRoute ->
            ( { model | currentPage = SettingsPage }, Cmd.none )

        SourceSelectionRoute ->
            ( { model | currentPage = SourceSelectionPage }, Cmd.none )

        BreathingMethodEditRoute id ->
            ( { model | currentPage = BreathingMethodEditPage id }, Cmd.none )


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
                ( newSessionModel, cmd ) =
                    SessionPage.update duration model.key msg sessionModel
            in
            ( { model | currentPage = PresetSessionPage duration newSessionModel }
            , Cmd.map PresetSessionPageMsg cmd
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
                ( newSessionModel, cmd ) =
                    SessionPage.update duration model.key msg sessionModel
            in
            ( { model | currentPage = ManualSessionPage duration newSessionModel }
            , Cmd.map ManualSessionPageMsg cmd
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
        PresetSessionCompletionPage duration id ->
            let
                cmd =
                    SessionCompletionPage.update model.key msg
            in
            ( { model | currentPage = PresetSessionCompletionPage duration id }
            , Cmd.map PresetSessionCompletionPageMsg cmd
            )

        _ ->
            ( model, Cmd.none )


{-| カスタムセッション完了画面のメッセージを処理する
-}
handleManualSessionCompletionPageMsg : SessionCompletionPage.Msg -> Model -> ( Model, Cmd Msg )
handleManualSessionCompletionPageMsg msg model =
    case model.currentPage of
        ManualSessionCompletionPage duration ->
            let
                cmd =
                    SessionCompletionPage.update model.key msg
            in
            ( { model | currentPage = ManualSessionCompletionPage duration }
            , Cmd.map ManualSessionCompletionPageMsg cmd
            )

        _ ->
            ( model, Cmd.none )


{-| Queryの結果を処理する
-}
handleReceiveOkQueryResult : QueryResult -> Model -> ( Model, Cmd Msg )
handleReceiveOkQueryResult queryResult model =
    case queryResult of
        QueryResult.CategoryListResult categories ->
            ( { model | categories = categories }, Cmd.none )

        QueryResult.BreathingMethodListResult breathingMethods ->
            ( { model | breathingMethods = breathingMethods }, Cmd.none )

        QueryResult.SessionListResult sessions ->
            ( { model | recentSessions = sessions }, Cmd.none )

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
        ( newUuidGenerator, effect, maybeMsg ) =
            Uuid.update msg model.uuidGenerator

        cmd =
            Uuid.effectToCmd Ports.generateUuidValue effect

        newModel =
            { model | uuidGenerator = newUuidGenerator }
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
        , Html.map UuidMsg <| button [ onClick (Uuid.uuidGenerate GotUuid) ] [ text "Generate UUID" ]
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

        PresetSessionCompletionPage _ _ ->
            "セッション完了"

        ManualSessionCompletionPage _ ->
            "カスタムセッション完了"

        StatisticsPage ->
            "統計"

        SettingsPage ->
            "設定"

        SourceSelectionPage ->
            "ソース選択"

        BreathingMethodEditPage _ ->
            "呼吸法編集"

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
        span [ attribute "role" "category-title" ] [ text category.title ]
            :: children


{-| ホーム画面のビュー
-}
viewHome : { model | categories : List Category, breathingMethods : List BreathingMethod } -> Html Msg
viewHome model =
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
                            model.breathingMethods
                )
                model.categories
        , button [ attribute "aria-label" "add-new-breathing-method" ] [ text "新しい呼吸法を追加" ]
        ]


{-| 既存セッション準備画面のビュー
-}
viewPresetSessionPreparation : SessionPreparationPage.Model -> BreathingMethod -> Html SessionPreparationPage.Msg
viewPresetSessionPreparation model m =
    let
        txt =
            "準備画面 - ID: " ++ Uuid.toString m.id

        practiceStyle =
            Preset m

        route duration =
            PresetSessionRoute m.id (Just duration)
    in
    SessionPreparationPage.view
        { txt = txt
        , practiceStyle = practiceStyle
        , route = route
        }
        model


{-| カスタムセッション準備画面のビュー
-}
viewManualSessionPreparation : SessionPreparationPage.Model -> Html SessionPreparationPage.Msg
viewManualSessionPreparation model =
    let
        txt =
            "カスタム準備画面"

        practiceStyle =
            Manual

        route duration =
            ManualSessionRoute (Just duration)
                (Maybe.andThen Types.BreathingMethod.toInhaleDuration <| String.toInt model.inhaleDurationInput)
                (Maybe.andThen Types.BreathingMethod.toInhaleHoldDuration <| String.toInt model.inhaleHoldDurationInput)
                (Maybe.andThen Types.BreathingMethod.toExhaleDuration <| String.toInt model.exhaleDurationInput)
                (Maybe.andThen Types.BreathingMethod.toExhaleHoldDuration <| String.toInt model.exhaleHoldDurationInput)
    in
    SessionPreparationPage.view
        { txt = txt
        , practiceStyle = practiceStyle
        , route = route
        }
        model


{-| 既存セッション完了画面のビュー
-}
viewPresetSessionCompletion : BreathingMethodId -> Int -> Html Msg
viewPresetSessionCompletion id duration =
    SessionCompletionPage.view
        { txt = "完了画面 - ID: " ++ Uuid.toString id
        , duration = duration
        }
        |> Html.map PresetSessionCompletionPageMsg


{-| カスタムセッション完了画面のビュー
-}
viewManualSessionCompletion : Int -> Html Msg
viewManualSessionCompletion duration =
    SessionCompletionPage.view
        { txt = "カスタム完了画面"
        , duration = duration
        }
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
        , section [ attribute "role" "recent-7-days" ]
            [ span [ attribute "role" "recent-sets" ] [ text "10 セット" ]
            , span [ attribute "role" "recent-minutes" ] [ text "30 分" ]
            ]
        , section [ attribute "role" "total" ]
            [ span [ attribute "role" "total-sets" ] [ text "100 セット" ]
            , span [ attribute "role" "total-minutes" ] [ text "300 分" ]
            ]
        , section [ attribute "role" "total-practice-days" ]
            [ span [] []
            , span [] [ text "30 日" ]
            ]
        ]


{-| 設定画面のビュー
-}
viewSettings : Html Msg
viewSettings =
    div [ attribute "role" "settings" ] [ text "設定画面" ]


{-| ソース選択画面のビュー
-}
viewSourceSelection : Html Msg
viewSourceSelection =
    div [ attribute "role" "source-selection" ] [ text "ソース選択画面" ]


{-| 呼吸法編集画面のビュー
-}
viewEditingBreathingMethod : BreathingMethodId -> Html Msg
viewEditingBreathingMethod id =
    div [ attribute "role" "edit" ] [ text ("呼吸法編集 - ID: " ++ Uuid.toString id) ]


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
            List.Extra.find (\m -> m.id == id) model.breathingMethods
                |> Maybe.Extra.unwrap viewNotFound (viewPresetSessionPreparation prepareModel)
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

        PresetSessionCompletionPage duration id ->
            viewPresetSessionCompletion id duration

        ManualSessionCompletionPage duration ->
            viewManualSessionCompletion duration

        StatisticsPage ->
            viewStatistics

        SettingsPage ->
            viewSettings

        SourceSelectionPage ->
            viewSourceSelection

        BreathingMethodEditPage id ->
            viewEditingBreathingMethod id

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

        PresetSessionCompletionPage _ _ ->
            presetSessionCompletionSubscriptions

        ManualSessionCompletionPage _ ->
            manualSessionCompletionSubscriptions

        StatisticsPage ->
            statisticsSubscriptions

        SettingsPage ->
            settingsSubscriptions

        SourceSelectionPage ->
            sourceSelectionSubscriptions

        BreathingMethodEditPage _ ->
            breathingMethodEditSubscriptions

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
main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
