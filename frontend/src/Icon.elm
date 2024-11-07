module Icon exposing (Icon(..), view)

{-| このモジュールはアイコンの表示を提供します。

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
