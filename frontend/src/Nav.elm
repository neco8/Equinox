module Nav exposing (NavType(..), map, view)

{-|


## Nav

このモジュールは、ナビゲーションバーを作成するためのモジュールです。

戻るボタンが付いており、基本的に設定ページなどの一時的なモードに入り、戻る事があるときに使用されるBackNavと、Navが存在します。

@docs NavType view

-}

import Html exposing (Html, button, div, h1, header, nav, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Icon


{-| ナビゲーションバーの種類です。
-}
type NavType msg
    = Nav { goToSettings : msg }
    | BackNav { goBack : msg, title : String }


{-| map関数
-}
map : (a -> b) -> NavType a -> NavType b
map f navType =
    case navType of
        Nav msg ->
            Nav { goToSettings = f msg.goToSettings }

        BackNav msg ->
            BackNav { goBack = f msg.goBack, title = msg.title }


{-| ビュー
-}
view : NavType msg -> Html msg
view navType =
    case navType of
        Nav msg ->
            viewNav msg

        BackNav msg ->
            viewBackNav msg


{-| ナビゲーションのビュー
-}
viewNav : { goToSettings : msg } -> Html msg
viewNav { goToSettings } =
    nav [ class "bg-white shadow-sm px-4 py-3" ]
        [ div [ class "flex justify-end items-center space-x-4 max-w-2xl mx-auto w-full" ]
            [ viewStreak 30
            , button
                [ attribute "aria-label" "settings"
                , class "aspect-square h-10 p-2 hover:bg-gray-200 rounded-full"
                , onClick goToSettings
                ]
                [ Icon.view Icon.Settings ]
            ]
        ]


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


{-| 戻るナビゲーションのビュー
-}
viewBackNav : { goBack : msg, title : String } -> Html msg
viewBackNav { goBack, title } =
    nav []
        [ header [ class "bg-white shadow-sm" ]
            [ div [ class "h-16 flex items-center max-w-2xl mx-auto" ]
                [ button
                    [ class "mr-4 p-2 h-10 hover:bg-gray-100 rounded-full aspect-square transition-colors duration-200"
                    , onClick goBack
                    ]
                    [ Icon.view Icon.ChevronLeft
                    ]
                , h1 [ class "text-xl font-semibold text-gray-900" ]
                    [ text title
                    ]
                ]
            ]
        ]
