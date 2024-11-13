module Pages.BreathingMethodPage exposing
    ( PageAction(..), Model, Msg
    , init
    , update
    , view
    )

{-|


## Breathing Method Page

このモジュールは、呼吸法を編集するためのページを提供します。


### 型

@docs PageAction, Model, Msg


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
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onClick, onInput)
import JS.Ports as Ports
import List.Extra
import Maybe.Extra
import Route exposing (Route)
import Task
import Time
import Types.BreathingMethod exposing (BreathingMethod, BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name, fromExhaleDuration, fromExhaleHoldDuration, fromInhaleDuration, fromInhaleHoldDuration, fromName, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration, toName)
import Types.Category exposing (Category, CategoryId, Title, fromTitle, toTitle)
import Uuid exposing (Uuid, effectToCmd)


{-| ページアクション

呼吸法を編集もしくは追加するためのアクションを表します。

-}
type PageAction
    = Edit BreathingMethodId
    | Add (Maybe Name) (Maybe InhaleDuration) (Maybe InhaleHoldDuration) (Maybe ExhaleDuration) (Maybe ExhaleHoldDuration)


{-| モデル
-}
type alias Model =
    { pageAction : PageAction
    , inhaleDurationInput : String
    , inhaleHoldDurationInput : String
    , exhaleDurationInput : String
    , exhaleHoldDurationInput : String
    , nameInput : String
    , selectedCategory : Maybe CategoryId
    , categoryComboboxModel : Combobox.Model Msg CategoryId
    }


{-| 初期化関数
-}
init : List BreathingMethod -> PageAction -> ( Model, Cmd Msg )
init breathingMethods pageAction =
    let
        breathingMethod : { inhale : String, inhaleHold : String, exhale : String, exhaleHold : String, name : String }
        breathingMethod =
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
                                }
                            )
                        |> Maybe.withDefault
                            { name = ""
                            , inhale = ""
                            , inhaleHold = ""
                            , exhale = ""
                            , exhaleHold = ""
                            }

                Add name inhale inhaleHold exhale exhaleHold ->
                    { name = Maybe.withDefault "" <| Maybe.map fromName name
                    , inhale = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromInhaleDuration) inhale
                    , inhaleHold = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromInhaleHoldDuration) inhaleHold
                    , exhale = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromExhaleDuration) exhale
                    , exhaleHold = Maybe.withDefault "" <| Maybe.map (String.fromInt << fromExhaleHoldDuration) exhaleHold
                    }

        categoryComboboxModel =
            Combobox.init
                (Combobox.Config
                    Uuid.toString
                    (.value >> SelectCategory)
                    CreateNewCategory
                    CategoryComboboxMsg
                )
    in
    ( { pageAction = pageAction
      , inhaleDurationInput = breathingMethod.inhale
      , inhaleHoldDurationInput = breathingMethod.inhaleHold
      , exhaleDurationInput = breathingMethod.exhale
      , exhaleHoldDurationInput = breathingMethod.exhaleHold
      , nameInput = breathingMethod.name
      , selectedCategory = Nothing
      , categoryComboboxModel = categoryComboboxModel
      }
    , Cmd.none
    )


{-| メッセージ
-}
type Msg
    = InputInhaleDuration String
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


{-| アップデート関数
-}
update : Nav.Key -> Uuid.Registry msg -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg, Uuid.Registry msg )
update key registry toMsg msg model =
    case msg of
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
            let
                getBreathingMethod : Maybe (Uuid -> BreathingMethod)
                getBreathingMethod =
                    Just
                        (\name categoryId inhaleDuration inhaleHoldDuration exhaleDuration exhaleHoldDuration id ->
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
            in
            case getBreathingMethod of
                Just fn ->
                    let
                        tag =
                            "breathing-method-edit/breathing-method"

                        handler =
                            GotNewBreathingMethodId fn >> toMsg

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
                |> Cmd.map toMsg
            , registry
            )

        CategoryComboboxMsg comboboxMsg ->
            let
                ( newComboboxModel, cmd ) =
                    Combobox.update comboboxMsg model.categoryComboboxModel
            in
            ( { model | categoryComboboxModel = newComboboxModel }
            , cmd
                |> Cmd.map toMsg
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
                            GotNewCategoryId title >> toMsg

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
                |> Cmd.map toMsg
            , registry
            )

        NavigateToRoute route ->
            ( model, Nav.pushUrl key (Route.toString route), registry )


{-| ビュー
-}
view : List Category -> Model -> Html Msg
view categories model =
    div [ attribute "role" "edit" ]
        [ text "呼吸法編集 - ID: "
        , BreathingMethodDurationInput.view
            (BreathingMethodDurationInput.Config
                InputInhaleDuration
                InputInhaleHoldDuration
                InputExhaleDuration
                InputExhaleHoldDuration
            )
            model
        , input
            [ attribute "aria-label" "breathing-method-name-input"
            , onInput InputName
            , value model.nameInput
            ]
            []
        , Combobox.view { ariaLabel = "category-combobox" }
            (List.map
                (\c ->
                    Combobox.Option c.id (fromTitle c.title)
                )
                categories
            )
            model.categoryComboboxModel
        , button
            [ attribute "aria-label" "submit-breathing-method"
            , onClick Submit
            ]
            [ text "Submit" ]
        ]
