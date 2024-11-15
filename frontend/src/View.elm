module View exposing (View, map)

{-|


## View

このモジュールはビューの表示を提供します。

例えば、ページのレイアウトを定義する際に、ヘッダーやフッターを表示するかどうかを指定することができます。

ヘッダーやフッターはグローバルな要素であるため、その表示を上手に管理することが重要です。

-}

import Html exposing (Html)


{-| ビューの型エイリアス
-}
type alias View msg =
    { view : Html msg, nav : Bool, footer : Bool }


{-| ビューをマップする関数
-}
map : (a -> b) -> View a -> View b
map f view =
    { view = Html.map f view.view
    , nav = view.nav
    , footer = view.footer
    }
