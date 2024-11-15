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


{-| ビュー
-}
view : Icon -> Html msg
view icon =
    case icon of
        Flame ->
            div
                []
                [ text "🔥" ]
