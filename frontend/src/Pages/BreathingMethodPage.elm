module Pages.BreathingMethodPage exposing
    ( PageAction(..), Model, Msg
    , noOp
    , init
    , update
    , view
    , subscriptions
    )

{-|


## Breathing Method Page

このモジュールは、呼吸法を編集するためのページを提供します。


### 型

@docs PageAction, Model, Msg
@docs noOp


### 初期化関数

@docs init


### アップデート

@docs update


### ビュー

@docs view

-}

import BreathingMethodDurationInput
import Browser.Navigation as Nav
import Common.Combobox as Combobox
import Html exposing (Html, button, div, h1, input, li, text)
import Html.Attributes exposing (attribute, class, disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Icon exposing (Icon(..))
import JS.Ports as Ports
import List.Extra
import Maybe.Extra
import Modal
import Monocle.Lens
import Nav
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, fromName, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration, toName)
import Types.Category exposing (Category, CategoryId, Title, fromTitle, toTitle)
import Uuid exposing (Uuid, effectToCmd)
import View exposing (View)


{-| ページアクション

呼吸法を編集もしくは追加するためのアクションを表します。

    type PageAction
        = Edit BreathingMethodId
        | Add (Maybe Name) (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)

-}
type PageAction
    = Edit BreathingMethodId
    | Add (Maybe Name) (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)


{-| 内部モデル
-}
type alias InternalModel =
    { pageAction : PageAction
    , inhaleDurationInput : String
    , inhaleHoldDurationInput : String
    , exhaleDurationInput : String
    , exhaleHoldDurationInput : String
    , nameInput : String
    , selectedCategory : Maybe CategoryId
    , categoryComboboxModel : Combobox.Model InternalMsg CategoryId
    }


{-| ハンバーガーメニューモデル
-}
type alias HamburgerModel =
    { isOpen : Bool
    }


{-| モーダルのモデル
-}
type alias ModalModel =
    { isOpen : Bool
    }


{-| モデル

    type Model
        = ModelLoading PageAction { hamburger : HamburgerModel, modal : ModalModel }
        | ModelLoaded InternalModel { hamburger : HamburgerModel, modal : ModalModel }

-}
type Model
    = ModelLoading PageAction { hamburger : HamburgerModel, modal : ModalModel }
    | ModelLoaded InternalModel { hamburger : HamburgerModel, modal : ModalModel }


{-| モデルからハンバーガーモデルを取得するLens
-}
modelHamburegerModel : Monocle.Lens.Lens Model HamburgerModel
modelHamburegerModel =
    Monocle.Lens.Lens
        (\model ->
            case model of
                ModelLoading _ sub ->
                    sub.hamburger

                ModelLoaded _ sub ->
                    sub.hamburger
        )
        (\h m ->
            case m of
                ModelLoading pageAction sub ->
                    ModelLoading pageAction { sub | hamburger = h }

                ModelLoaded internal sub ->
                    ModelLoaded internal { sub | hamburger = h }
        )


{-| モデルからモーダルモデルを取得するLens
-}
modelModalmmodel : Monocle.Lens.Lens Model ModalModel
modelModalmmodel =
    Monocle.Lens.Lens
        (\model ->
            case model of
                ModelLoading _ { modal } ->
                    modal

                ModelLoaded _ { modal } ->
                    modal
        )
        (\m model ->
            case model of
                ModelLoading pageAction { hamburger } ->
                    ModelLoading pageAction { hamburger = hamburger, modal = m }

                ModelLoaded internal { hamburger } ->
                    ModelLoaded internal { hamburger = hamburger, modal = m }
        )


{-| ハンバーガーメニューモデルの初期値
-}
initHamburgerModel : HamburgerModel
initHamburgerModel =
    { isOpen = False }


{-| モーダルの初期値
-}
initModalModel : ModalModel
initModalModel =
    { isOpen = False }


{-| 初期化関数

呼吸法が取得済みなら、initしてよい。だが、取得が後からになる場合はupdateの中でも初期化する必要がある。

-}
init : RemoteData e { categories : List Category, breathingMethods : List BreathingMethod } -> Nav.Key -> PageAction -> ( Model, Cmd Msg )
init remote key pageAction =
    case remote of
        NotAsked ->
            ( ModelLoading pageAction
                { hamburger = initHamburgerModel
                , modal = initModalModel
                }
            , Cmd.none
            )

        Loading ->
            ( ModelLoading pageAction
                { hamburger = initHamburgerModel
                , modal = initModalModel
                }
            , Cmd.none
            )

        Failure _ ->
            -- ホーム画面へ戻る。 - [ ] TODO: エラーメッセージを表示する
            ( ModelLoading pageAction
                { hamburger = initHamburgerModel
                , modal = initModalModel
                }
            , NavigateToRoute Route.HomeRoute
                |> InternalMsg
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        Success data ->
            let
                ( newModel, cmd ) =
                    initInternal data.breathingMethods data.categories key pageAction
            in
            ( ModelLoaded newModel
                { hamburger = initHamburgerModel
                , modal = initModalModel
                }
            , cmd
            )


{-| 初期化関数

内部で実際に行っているもの

-}
initInternal : List BreathingMethod -> List Category -> Nav.Key -> PageAction -> ( InternalModel, Cmd Msg )
initInternal breathingMethods categories key pageAction =
    let
        mbreathingMethod =
            case pageAction of
                Edit id ->
                    List.Extra.find (\bm -> bm.id == id) breathingMethods
                        |> Maybe.map
                            (\bm ->
                                { name = fromName bm.name
                                , inhale = String.fromInt <| fromInhaleDuration bm.inhaleDuration
                                , inhaleHold = String.fromInt <| fromInhaleHoldDuration bm.inhaleHoldDuration
                                , exhale = String.fromInt <| fromExhaleDuration bm.exhaleDuration
                                , exhaleHold = String.fromInt <| fromExhaleHoldDuration bm.exhaleHoldDuration
                                , categoryId = Just bm.categoryId
                                }
                            )

                Add name inhale inhaleHold exhale exhaleHold ->
                    Just
                        { name = Maybe.withDefault "" <| Maybe.map fromName name
                        , inhale = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromInhaleDuration) inhale
                        , inhaleHold = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromInhaleHoldDuration) inhaleHold
                        , exhale = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromExhaleDuration) exhale
                        , exhaleHold = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromExhaleHoldDuration) exhaleHold
                        , categoryId = Nothing
                        }

        categoryComboboxModel =
            Combobox.init
                (Combobox.Config
                    Uuid.toString
                    (.value >> SelectCategory)
                    (toTitle >> Maybe.Extra.isJust)
                    CreateNewCategory
                    CategoryComboboxMsg
                    (case pageAction of
                        Edit id ->
                            List.Extra.find (.id >> (==) id) breathingMethods
                                |> Maybe.map .categoryId
                                |> Maybe.andThen (\cid -> List.Extra.find (.id >> (==) cid) categories)
                                |> Maybe.map (\c -> Combobox.Option c.id (fromTitle c.title))

                        Add _ _ _ _ _ ->
                            Nothing
                    )
                )
    in
    case mbreathingMethod of
        Just breathingMethod ->
            ( { pageAction = pageAction
              , inhaleDurationInput = breathingMethod.inhale
              , inhaleHoldDurationInput = breathingMethod.inhaleHold
              , exhaleDurationInput = breathingMethod.exhale
              , exhaleHoldDurationInput = breathingMethod.exhaleHold
              , nameInput = breathingMethod.name
              , selectedCategory = breathingMethod.categoryId
              , categoryComboboxModel = categoryComboboxModel
              }
            , Cmd.none
            )

        Nothing ->
            ( { pageAction = pageAction
              , inhaleDurationInput = ""
              , inhaleHoldDurationInput = ""
              , exhaleDurationInput = ""
              , exhaleHoldDurationInput = ""
              , nameInput = ""
              , selectedCategory = Nothing
              , categoryComboboxModel = categoryComboboxModel
              }
            , HomeRoute
                |> Route.toString
                |> Nav.replaceUrl key
            )


{-| ハンバーガーメニューメッセージ

    type HamburgerMenuMsg
        = ToggleHamburgerMenu
        | CloseHamburgerMenu

-}
type HamburgerMenuMsg
    = ToggleHamburgerMenu
    | CloseHamburgerMenu


{-| モーダルメッセージ

    type ModalMsg
        = OpenDeleteBreathingMethodModal
        | CloseDeleteBreathingMethodModal

-}
type ModalMsg
    = OpenDeleteBreathingMethodModal
    | CloseDeleteBreathingMethodModal


{-| メッセージ

    type Msg
        = HamburgerMenuMsg HamburgerMenuMsg
        | ModalMsg ModalMsg
        | InternalMsg InternalMsg

-}
type Msg
    = HamburgerMenuMsg HamburgerMenuMsg
    | ModalMsg ModalMsg
    | InternalMsg InternalMsg


{-| 内部メッセージ

    type InternalMsg
        = NoOp
        | InputInhaleDuration String
        | InputInhaleHoldDuration String
        | InputExhaleDuration String
        | InputExhaleHoldDuration String
        | InputName String
        | Submit
        | CategoryComboboxMsg (Combobox.Msg CategoryId)
        | CreateNewCategory String
        | GotNewCategoryId Title Uuid
        | SelectCategory CategoryId
        | GotCreatedAt Time.Posix
        | GotNewBreathingMethodId (Uuid -> BreathingMethod) Uuid
        | NavigateToRoute Route
        | GoBack
        | ClickOpenDeleteBreathingMethodModal
        | DeleteBreathingMethod
        | ReceiveDeleteBreathingMethodResult Bool

-}
type InternalMsg
    = NoOp
    | InputInhaleDuration String
    | InputInhaleHoldDuration String
    | InputExhaleDuration String
    | InputExhaleHoldDuration String
    | InputName String
    | Submit
    | CategoryComboboxMsg (Combobox.Msg CategoryId)
    | CreateNewCategory String
    | GotNewCategoryId Title Uuid
    | SelectCategory CategoryId
    | GotCreatedAt Time.Posix
    | GotNewBreathingMethodId (Uuid -> BreathingMethod) Uuid
    | NavigateToRoute Route
    | GoBack
    | ClickOpenDeleteBreathingMethodModal
    | DeleteBreathingMethod
    | ReceiveDeleteBreathingMethodResult Bool


{-| メッセージ: NoOp

画面描画更新用のメッセージ

-}
noOp : Msg
noOp =
    InternalMsg NoOp


{-| ハンバーガーメニューのアップデート関数
-}
updateHamburgerModel : HamburgerMenuMsg -> Model -> Model
updateHamburgerModel msg model =
    case msg of
        ToggleHamburgerMenu ->
            Monocle.Lens.modify modelHamburegerModel (\m -> { m | isOpen = not m.isOpen }) model

        CloseHamburgerMenu ->
            Monocle.Lens.modify modelHamburegerModel (\m -> { m | isOpen = False }) model


{-| モーダルのアップデート関数
-}
updateModalModel : ModalMsg -> Model -> Model
updateModalModel msg model =
    case msg of
        OpenDeleteBreathingMethodModal ->
            Monocle.Lens.modify modelModalmmodel (\m -> { m | isOpen = True }) model

        CloseDeleteBreathingMethodModal ->
            Monocle.Lens.modify modelModalmmodel (\m -> { m | isOpen = False }) model


{-| アップデート関数
-}
update : RemoteData e { categories : List Category, breathingMethods : List BreathingMethod } -> Nav.Key -> Uuid.Registry msg -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg, Uuid.Registry msg )
update remote key registry toMsg msg model =
    case msg of
        HamburgerMenuMsg subMsg ->
            ( updateHamburgerModel subMsg model, Cmd.none, registry )

        ModalMsg subMsg ->
            ( updateModalModel subMsg model, Cmd.none, registry )

        InternalMsg subMsg ->
            case model of
                ModelLoading pageAction hamburgerModel ->
                    case remote of
                        Success data ->
                            let
                                ( newModel, cmd ) =
                                    initInternal data.breathingMethods data.categories key pageAction
                            in
                            ( ModelLoaded newModel hamburgerModel, Cmd.map toMsg cmd, registry )

                        Failure _ ->
                            ( model
                              -- ホームへ遷移する。 - [ ] TODO: エラーメッセージを表示する
                            , NavigateToRoute HomeRoute
                                |> (InternalMsg >> toMsg)
                                |> always
                                |> Task.perform
                                |> (|>) Time.now
                            , registry
                            )

                        Loading ->
                            ( model, Cmd.none, registry )

                        NotAsked ->
                            ( model, Cmd.none, registry )

                ModelLoaded internal hamburgerModel ->
                    let
                        ( newInternal, cmd, newRegistry ) =
                            updateInternal key registry toMsg subMsg internal
                    in
                    ( ModelLoaded newInternal hamburgerModel, cmd, newRegistry )


{-| 呼吸法を作成する関数
-}
createBreathingMethod : InternalModel -> Maybe (Time.Posix -> Uuid -> BreathingMethod)
createBreathingMethod model =
    Just
        (\name categoryId inhaleDuration inhaleHoldDuration exhaleDuration exhaleHoldDuration createdAt id ->
            BreathingMethod id
                name
                categoryId
                createdAt
                inhaleDuration
                inhaleHoldDuration
                exhaleDuration
                exhaleHoldDuration
        )
        |> Maybe.Extra.andMap (toName model.nameInput)
        |> Maybe.Extra.andMap model.selectedCategory
        |> Maybe.Extra.andMap (Maybe.andThen toInhaleDuration <| String.toInt model.inhaleDurationInput)
        |> Maybe.Extra.andMap (Maybe.andThen toInhaleHoldDuration <| String.toInt model.inhaleHoldDurationInput)
        |> Maybe.Extra.andMap (Maybe.andThen toExhaleDuration <| String.toInt model.exhaleDurationInput)
        |> Maybe.Extra.andMap (Maybe.andThen toExhaleHoldDuration <| String.toInt model.exhaleHoldDurationInput)


{-| 内部で利用されているアップデート関数
-}
updateInternal : Nav.Key -> Uuid.Registry msg -> (Msg -> msg) -> InternalMsg -> InternalModel -> ( InternalModel, Cmd msg, Uuid.Registry msg )
updateInternal key registry toMsg msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, registry )

        InputInhaleDuration duration ->
            ( { model | inhaleDurationInput = duration }, Cmd.none, registry )

        InputInhaleHoldDuration duration ->
            ( { model | inhaleHoldDurationInput = duration }, Cmd.none, registry )

        InputExhaleDuration duration ->
            ( { model | exhaleDurationInput = duration }, Cmd.none, registry )

        InputExhaleHoldDuration duration ->
            ( { model | exhaleHoldDurationInput = duration }, Cmd.none, registry )

        InputName name ->
            ( { model | nameInput = name }, Cmd.none, registry )

        GotCreatedAt createdAt ->
            case
                createBreathingMethod model
                    |> Maybe.Extra.andMap (Just createdAt)
            of
                Just fn ->
                    let
                        tag =
                            "breathing-method-edit/breathing-method"

                        handler =
                            GotNewBreathingMethodId fn >> InternalMsg >> toMsg

                        uuidMsg =
                            Uuid.uuidGenerate tag handler

                        ( newRegistry, effect, maybeMsg ) =
                            Uuid.update uuidMsg registry
                    in
                    ( model
                    , Cmd.batch
                        [ effectToCmd Ports.generateUuidValue effect
                        , maybeMsg
                            |> Maybe.map
                                (always
                                    >> Task.perform
                                    >> (|>) Time.now
                                )
                            |> Maybe.withDefault Cmd.none
                        ]
                    , newRegistry
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    , registry
                    )

        Submit ->
            ( model
            , Task.perform GotCreatedAt Time.now
                |> Cmd.map (InternalMsg >> toMsg)
            , registry
            )

        CategoryComboboxMsg comboboxMsg ->
            let
                ( newComboboxModel, cmd ) =
                    Combobox.update comboboxMsg model.categoryComboboxModel
            in
            ( { model | categoryComboboxModel = newComboboxModel }
            , cmd
                |> Cmd.map (InternalMsg >> toMsg)
            , registry
            )

        CreateNewCategory str ->
            case toTitle str of
                -- [ ] TODO: ユーザーバリデーション
                Just title ->
                    let
                        tag =
                            "breathing-method/category"

                        handler =
                            GotNewCategoryId title >> InternalMsg >> toMsg

                        uuidMsg =
                            Uuid.uuidGenerate tag handler

                        ( newRegistry, effect, maybeMsg ) =
                            Uuid.update uuidMsg registry

                        cmd =
                            effectToCmd Ports.generateUuidValue effect
                    in
                    ( model
                    , Cmd.batch
                        [ cmd
                        , maybeMsg
                            |> Maybe.map
                                (always
                                    >> Task.perform
                                    >> (|>) Time.now
                                )
                            |> Maybe.withDefault Cmd.none
                        ]
                    , newRegistry
                    )

                Nothing ->
                    ( model, Cmd.none, registry )

        GotNewCategoryId title categoryId ->
            ( { model | selectedCategory = Just categoryId }
            , Ports.saveCategoryValue (Category categoryId title)
            , registry
            )

        SelectCategory categoryId ->
            ( { model | selectedCategory = Just categoryId }, Cmd.none, registry )

        GotNewBreathingMethodId getBreathingMethod breathingMethodId ->
            ( model
            , Cmd.batch
                [ Ports.saveBreathingMethodValue (getBreathingMethod breathingMethodId)
                , NavigateToRoute Route.HomeRoute
                    |> always
                    |> Task.perform
                    |> (|>) Time.now
                ]
                |> Cmd.map (InternalMsg >> toMsg)
            , registry
            )

        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route), registry )

        GoBack ->
            ( model, Nav.back key 1, registry )

        ClickOpenDeleteBreathingMethodModal ->
            ( model
            , [ ModalMsg OpenDeleteBreathingMethodModal
              , HamburgerMenuMsg CloseHamburgerMenu
              ]
                |> List.map (toMsg >> always >> Task.perform >> (|>) Time.now)
                |> Cmd.batch
            , registry
            )

        DeleteBreathingMethod ->
            case model.pageAction of
                Edit id ->
                    ( model
                    , Ports.deleteBreathingMethodValue id
                    , registry
                    )

                Add _ _ _ _ _ ->
                    ( model, Cmd.none, registry )

        ReceiveDeleteBreathingMethodResult isSuccess ->
            if isSuccess then
                ( model
                , NavigateToRoute Route.HomeRoute
                    |> InternalMsg
                    |> toMsg
                    |> always
                    |> Task.perform
                    |> (|>) Time.now
                , registry
                )

            else
                ( model, Cmd.none, registry )


{-| メニューアイテム
-}
menuItem : String -> Msg -> Html Msg
menuItem label msg =
    li
        [ class "block px-4 py-2 text-gray-800 hover:bg-gray-100 rounded-lg transition-colors duration-200"
        , onClick msg
        ]
        [ text label ]


{-| ビュー
-}
view : RemoteData e (List Category) -> Model -> View Msg
view remote model =
    let
        pageAction =
            case model of
                ModelLoading action _ ->
                    action

                ModelLoaded loaded _ ->
                    loaded.pageAction
    in
    { nav =
        Nav.initialConfig
            |> Nav.withGoBack (InternalMsg GoBack)
            |> Nav.withTitle
                ("呼吸法"
                    ++ (case pageAction of
                            Edit _ ->
                                "編集"

                            Add _ _ _ _ _ ->
                                "追加"
                       )
                )
            |> (case pageAction of
                    Edit _ ->
                        Nav.withRightTop
                            [ div [ class "w-full justify-end flex relative" ]
                                [ Nav.viewHamburger (HamburgerMenuMsg ToggleHamburgerMenu)

                                -- 背景オーバーレイ
                                , if (modelHamburegerModel.get model).isOpen then
                                    div
                                        [ class "fixed inset-0 z-10"
                                        , onClick (HamburgerMenuMsg CloseHamburgerMenu)
                                        ]
                                        []

                                  else
                                    text ""

                                -- メニュー本体
                                , div
                                    [ class "absolute right-0 top-full mt-2 w-48 rounded-xl bg-white shadow-lg ring-1 ring-gray-200 transition-all duration-200 z-20"
                                    , if (modelHamburegerModel.get model).isOpen then
                                        class "opacity-100 transform scale-100"

                                      else
                                        class "opacity-0 transform scale-95 pointer-events-none"
                                    ]
                                    [ div [ class "py-1" ]
                                        [ menuItem "削除する"
                                            (InternalMsg ClickOpenDeleteBreathingMethodModal)
                                        ]
                                    ]
                                ]
                            ]

                    Add _ _ _ _ _ ->
                        identity
               )
            |> Just
    , footer = False
    , view =
        div
            [ attribute "role" "edit"
            , class "max-w-2xl mx-auto"
            ]
            (List.concat
                [ [ h1 [ class "text-2xl font-medium text-gray-700 mb-8 text-center" ]
                        [ text <|
                            "呼吸法"
                                ++ (case pageAction of
                                        Edit _ ->
                                            "編集"

                                        Add _ _ _ _ _ ->
                                            "追加"
                                   )
                        ]
                  ]
                , case model of
                    ModelLoading _ _ ->
                        [ text "loading..." ]

                    ModelLoaded loaded _ ->
                        [ Html.map InternalMsg <|
                            BreathingMethodDurationInput.view
                                (BreathingMethodDurationInput.Config
                                    InputInhaleDuration
                                    InputInhaleHoldDuration
                                    InputExhaleDuration
                                    InputExhaleHoldDuration
                                )
                                loaded
                        , div [ class "space-y-4 px-4" ]
                            [ div [ class "relative" ]
                                [ Html.map InternalMsg <|
                                    input
                                        [ attribute "aria-label" "breathing-method-name-input"
                                        , onInput InputName
                                        , value loaded.nameInput
                                        , class "w-full py-3 px-4 bg-transparent text-sm text-gray-800 border-b-2n border-gray-200 focus:border-blue-400 focus:outline-none mt-8 border-b-2 "
                                        , placeholder "呼吸法の名前を入力"
                                        ]
                                        []
                                ]
                            , case remote of
                                Success categories ->
                                    Html.map InternalMsg <|
                                        Combobox.view { ariaLabel = "category-combobox" }
                                            (List.map
                                                (\c ->
                                                    Combobox.Option c.id (fromTitle c.title)
                                                )
                                                categories
                                            )
                                            loaded.categoryComboboxModel

                                Failure _ ->
                                    text "category loading failure"

                                Loading ->
                                    text "category loading..."

                                NotAsked ->
                                    text "category not asked"
                            , div [ class "pt-8" ]
                                [ Html.map InternalMsg <|
                                    button
                                        [ attribute "aria-label" "submit-breathing-method"
                                        , onClick Submit
                                        , createBreathingMethod loaded
                                            |> Maybe.Extra.isNothing
                                            |> disabled
                                        , class "w-full py-4 bg-gradient-to-r from-blue-500 to-purple-500 text-white rounded-xl font-medium text-lg shadow-md hover:from-blue-600 hover:to-purple-600 disabled:from-gray-300 disabled:to-gray-400 disabled:cursor-not-allowed transition-all duration-200"
                                        ]
                                        [ text "保存" ]
                                ]
                            ]
                        ]
                , [ Modal.view
                        (Modal.Config
                            (modelModalmmodel.get model).isOpen
                            (ModalMsg CloseDeleteBreathingMethodModal)
                            (div [ class "" ]
                                [ text "呼吸法を削除しますか？"
                                , div [ class "mt-6 flex justify-end space-x-2" ]
                                    [ button
                                        [ class "px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md shadow-sm hover:bg-gray-50"
                                        , onClick (ModalMsg CloseDeleteBreathingMethodModal)
                                        ]
                                        [ text "キャンセル"
                                        ]
                                    , button
                                        [ class "px-4 py-2 text-sm font-medium text-white bg-red-600 rounded-md shadow-sm hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500"
                                        , onClick (InternalMsg DeleteBreathingMethod)
                                        ]
                                        [ text "削除する"
                                        ]
                                    ]
                                ]
                            )
                            "呼吸法削除"
                            Modal.Medium
                        )
                  ]
                ]
            )
    }


{-| サブスクリプション
-}
subscriptions : Sub Msg
subscriptions =
    Ports.subscribeToDeleteBreathingMethodResult
        (ReceiveDeleteBreathingMethodResult >> InternalMsg)
