module Icon exposing (Icon(..), view)

{-|


## Icon

このモジュールはアイコンの表示を提供します。

このモジュールにアイコンを集約することで、アイコンの表示を一元管理できます。アイコンはいずれ自作したものに置き換えることを想定しています。

@docs Icon, view

-}

import Html exposing (Html, div, text)


{-| アイコンの種類を定義します。
-}
type Icon
    = Flame
    | Settings
    | Plus
    | Home
    | Play
    | Statistics
    | Timer
    | Wind
    | Pause


{-| ビュー
-}
view : Icon -> Html msg
view icon =
    case icon of
        Flame ->
            div
                []
                [ text "🔥" ]

        Settings ->
            div
                []
                [ text "⚙️" ]

        Plus ->
            div
                []
                [ text "➕" ]

        Home ->
            div
                []
                [ text "🏠" ]

        Play ->
            div
                []
                [ text "▶️" ]

        Statistics ->
            div
                []
                [ text "📊" ]

        Timer ->
            div
                []
                [ text "⏱" ]

        Wind ->
            div
                []
                [ text "💨" ]

        Pause ->
            div
                []
                [ text "⏸" ]
