module Modal exposing
    ( Config, Size(..)
    , view
    )

{-|


## Modal

このモジュールは、モーダルを表示するためのモジュールです。


### 基本型

@docs Config, Size


### ビュー

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon


{-| モーダルの設定
-}
type alias Config msg =
    { isOpen : Bool
    , onClose : msg
    , content : Html msg
    , title : String
    , size : Size
    }


{-| モーダルのサイズ

    type Size
        = Small
        | Medium
        | Large

-}
type Size
    = Small
    | Medium
    | Large


{-| モーダルのサイズを指定するクラス
-}
sizeToClass : Size -> Attribute msg
sizeToClass size =
    case size of
        Small ->
            class "max-w-sm"

        Medium ->
            class "max-w-lg"

        Large ->
            class "max-w-2xl"


{-| モーダルを表示する
-}
view : Config msg -> Html msg
view config =
    if config.isOpen then
        div
            [ class "fixed inset-0 z-50 flex items-center justify-center transition-opacity duration-300"
            ]
            [ div
                [ class "fixed inset-0 bg-black bg-opacity-50 transition-opacity duration-300"
                , onClick config.onClose
                ]
                []
            , div
                [ class "bg-white rounded-lg shadow-xl p-6 w-full mx-4 relative z-50 transform transition-all duration-300"
                , sizeToClass config.size
                ]
                [ div [ class "flex justify-between items-center mb-4" ]
                    [ h2 [ class "text-xl font-bold" ] [ text config.title ]
                    , button
                        [ onClick config.onClose
                        , class "text-gray-500 hover:text-gray-700 transition-colors"
                        ]
                        [ Icon.view Icon.Close ]
                    ]
                , div [ class "overflow-y-auto" ] [ config.content ]
                ]
            ]

    else
        text ""
