module Common.Combobox exposing (Config, Model, Msg, Option, init, update, view)

{-|


## Combobox

このモジュールは、コンボボックスを作成するためのモジュールです。


### 使い方

親コンポーネントでは、以下のように使います。

    type alias Model =
        { ...
        , comboboxModel : Combobox.Model Msg
        }

    type Msg
        = ...
        | ComboboxSelect Combobox.Option
        | CreateNewOption String
        | ComboboxMsg Combobox.Msg

    init : () -> ( Model, Cmd Msg )
    init _ =
        let
            comboboxModel =
                Combobox.init
                    { onSelect = ComboboxSelect
                    , onCreateNew = CreateNewOption
                    , toMsg = ComboboxMsg
                    }
            ...
        in
        ...

    view : Model -> Html Msg
    view model =
        Combobox.view
            (List.map
                (\option -> Combobox.Option (Uuid.toString option.id) (fromLabel option.label))
                model.options
            )
            model.comboboxModel

-}

import Html exposing (Html, button, div, input, option, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Task
import Time


{-| Combobox に使用されるOptionの型です。
-}
type alias Option a =
    { value : a
    , label : String
    }


{-| Combobox に利用される Model です。
-}
type alias Model msg a =
    { inputValue : String
    , isOpen : Bool
    , selectedOption : Maybe (Option a)
    , config : Config msg a
    }


{-| Combobox の初期値を設定します。
-}
init : Config msg a -> Model msg a
init config =
    { inputValue = ""
    , isOpen = False
    , selectedOption = Nothing
    , config = config
    }


{-| Combobox で発生するメッセージです。
-}
type Msg a
    = InputChanged String
    | OptionSelected (Option a)
    | OpenDropdown
    | CloseDropdown
    | ClickCreateNew


{-| Comboboxの設定を行います。
-}
type alias Config msg a =
    { toString : a -> String
    , onSelect : Option a -> msg
    , canCreateNew : String -> Bool
    , onCreateNew :
        String
        -> msg -- 親コンポーネントが新規作成をハンドルする
    , toMsg : Msg a -> msg
    }


{-| Combobox のビューを作成します。
-}
view : { ariaLabel : String } -> List (Option a) -> Model msg a -> Html msg
view { ariaLabel } options model =
    div
        [ class "relative"
        , attribute "role" "combobox"
        , attribute "aria-label" ariaLabel
        , onBlur (CloseDropdown |> model.config.toMsg) -- TODO: 外側を押したら閉じるようにする
        ]
        [ input
            [ value model.inputValue
            , attribute "role" "combo-input"
            , onInput (InputChanged >> model.config.toMsg)
            , type_ "text"
            , onClick (OpenDropdown |> model.config.toMsg)
            , class "w-full p-3 border border-gray-200 rounded-lg focus:outline-none focus:border-transparent focus:ring-2 focus:ring-orange-500 caret-orange-500"
            ]
            []
        , if model.isOpen then
            viewDropdown options model

          else
            text ""
        ]


{-| Combobox 下部のドロップダウンを作成します。
-}
viewDropdown : List (Option a) -> Model msg a -> Html msg
viewDropdown options model =
    let
        hasExactMatch =
            List.any (\opt -> String.toLower opt.label == String.toLower model.inputValue)
                options
    in
    Html.Keyed.node "div"
        [ class "absolute mt-2 w-full bg-white rounded-lg border border-gray-100 overflow-hidden z-50"
        , attribute "role" "listbox"
        ]
        (List.concat
            [ List.map (viewOption model) options
            , if (not hasExactMatch && String.length model.inputValue > 0) || List.isEmpty options then
                [ ( "create-new", viewCreateNew model.config model.inputValue ) ]

              else
                []
            ]
        )


{-| Comobobox のオプションを作成します。
-}
viewOption : { model | selectedOption : Maybe (Option a), config : Config msg a } -> Option a -> ( String, Html msg )
viewOption { selectedOption, config } opt =
    ( config.toString opt.value
    , div
        [ onClick (OptionSelected opt |> config.toMsg)
        , attribute "role" "option"
        , attribute "data-id" ("option-" ++ config.toString opt.value)
        , class "px-4 py-3 hover:bg-gray-50 cursor-pointer flex justify-between items-center" -- TODO: groupってなんだ？
        , if selectedOption == Just opt then
            class "bg-gray-50"

          else
            class ""
        ]
        [ text opt.label ]
    )


{-| Combobox の新規作成行を作成します。
-}
viewCreateNew : Config msg a -> String -> Html msg
viewCreateNew config inputValue =
    button
        [ onClick (ClickCreateNew |> config.toMsg)
        , attribute "role" "create-new-option"
        , class "px-4 py-3 hover:bg-gray-50 cursor-pointer flex justify-between items-center disabled:bg-gray-200"
        , disabled (not (config.canCreateNew inputValue))
        ]
        [ text ("新規作成: " ++ inputValue) ]


{-| Combobox のアップデートを行います。
-}
update : Msg a -> Model msg a -> ( Model msg a, Cmd msg )
update msg model =
    case msg of
        InputChanged newValue ->
            ( { model
                | inputValue = newValue
                , isOpen = True
              }
            , Cmd.none
            )

        OptionSelected option ->
            ( { model
                | selectedOption = Just option
                , inputValue = option.label
                , isOpen = False
              }
            , model.config.onSelect option
                |> always
                |> Task.perform
                |> (|>) Time.now
            )

        OpenDropdown ->
            ( { model | isOpen = True }, Cmd.none )

        CloseDropdown ->
            ( { model | isOpen = False }, Cmd.none )

        ClickCreateNew ->
            ( model
            , [ model.config.onCreateNew model.inputValue
              , CloseDropdown |> model.config.toMsg
              ]
                |> List.map
                    (always
                        >> Task.perform
                        >> (|>) Time.now
                    )
                |> Cmd.batch
            )
