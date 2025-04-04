module View.List exposing (viewList)

{-|


## View.List

このモジュールは、空の状態、コンテナ、アイテムビューをカスタマイズ可能なリスト表示ユーティリティを提供します。


### リストビュー

@docs viewList

-}

import Html exposing (Html)


{-| アイテムのリストを、カスタマイズ可能な空の状態、コンテナ、アイテムビューで表示します。

    viewList
        { emptyState = text "アイテムがありません"
        , container = \items -> ul [ class "item-list" ] items
        , item = \item -> li [] [ text item.name ]
        }
        myItems


## 引数

  - `config` - リストビューの設定:
      - `emptyState` - リストが空の場合に表示するビュー
      - `container` - リストアイテムをラップするコンテナ関数
      - `item` - 個々のアイテムをレンダリングする関数


## 戻り値

  - リストが空の場合、指定された `emptyState` を返します
  - リストにアイテムがある場合、各アイテムを `item` 関数でマッピングし、`container` でラップします

-}
viewList : { views | emptyState : Html msg, container : List (Html msg) -> Html msg, item : a -> Html msg } -> List a -> Html msg
viewList config items =
    if List.isEmpty items then
        config.emptyState

    else
        items
            |> List.map config.item
            |> config.container
