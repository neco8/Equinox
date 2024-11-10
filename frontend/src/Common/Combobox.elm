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

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Task
import Time


{-| Combobox に使用されるOptionの型です。
-}
type alias Option =
    { value : String
    , label : String
    }


{-| Combobox に利用される Model です。
-}
type alias Model msg =
    { inputValue : String
    , isOpen : Bool
    , selectedOption : Maybe Option
    , config : Config msg
    }


{-| Combobox の初期値を設定します。
-}
init : Config msg -> Model msg
init config =
    { inputValue = ""
    , isOpen = False
    , selectedOption = Nothing
    , config = config
    }


{-| Combobox で発生するメッセージです。
-}
type Msg
    = InputChanged String
    | OptionSelected Option
    | OpenDropdown
    | CloseDropdown
    | ClickCreateNew



-- Config type to allow parent to specify behavior


{-| Comboboxの設定を行います。
-}
type alias Config msg =
    { onSelect : Option -> msg
    , onCreateNew :
        String
        -> msg -- 親コンポーネントが新規作成をハンドルする
    , toMsg : Msg -> msg
    }


{-| Combobox のビューを作成します。
-}
view : List Option -> Model msg -> Html msg
view options model =
    div
        [ class "relative"
        , onBlur (CloseDropdown |> model.config.toMsg) -- TODO: すべて関係なくなったら閉じる。外側を閉じたら。
        ]
        [ input
            [ value model.inputValue
            , onInput (InputChanged >> model.config.toMsg)
            , type_ "text"
            , onFocus (OpenDropdown |> model.config.toMsg)
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
viewDropdown : List Option -> Model msg -> Html msg
viewDropdown options model =
    let
        hasExactMatch =
            List.any (\opt -> String.toLower opt.label == String.toLower model.inputValue)
                options
    in
    Html.Keyed.node "div"
        [ class "absolute mt-2 w-full bg-white rounded-lg border border-gray-100 overflow-hidden z-50" ]
        (List.concat
            [ List.map (viewOption model) options
            , if not hasExactMatch && String.length model.inputValue > 0 then
                [ ( "create-new", viewCreateNew model.config model.inputValue ) ]

              else
                []
            ]
        )


{-| Comobobox のオプションを作成します。
-}
viewOption : { model | selectedOption : Maybe Option, config : Config msg } -> Option -> ( String, Html msg )
viewOption { selectedOption, config } option =
    ( option.value
    , div
        [ onClick (OptionSelected option |> config.toMsg)
        , class "px-4 py-3 hover:bg-gray-50 cursor-pointer flex justify-between items-center" -- TODO: groupってなんだ？
        , if selectedOption == Just option then
            class "bg-gray-50"

          else
            class ""
        ]
        [ text option.label ]
    )


{-| Combobox の新規作成行を作成します。
-}
viewCreateNew : Config msg -> String -> Html msg
viewCreateNew config inputValue =
    div
        [ onClick (ClickCreateNew |> config.toMsg)
        , class "px-4 py-3 hover:bg-gray-50 cursor-pointer flex justify-between items-center group"
        ]
        [ text ("新規作成: " ++ inputValue) ]


{-| Combobox のアップデートを行います。
-}
update : Msg -> Model msg -> ( Model msg, Cmd msg )
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
