module Types.Category exposing (..)

{-| このモジュールは、呼吸法に関連する`Category`の型と関数を定義します。
`Category`にはカテゴリID、タイトルなどが含まれます。
呼吸法は特定のカテゴリに関連付けられることができ、カテゴリごとに呼吸法を分類するために使用されます。


### 型

@docs CategoryId, Title, Category


### TODO

  - [ ] TODO: `Title`をOpaque Typeに変更し、直接のアクセスを制限する。

-}

import Uuid exposing (Uuid)


{-| カテゴリーのID（UUID）。
-}
type alias CategoryId =
    Uuid


{-| カテゴリのタイトル。
-}
type alias Title =
    String


{-| タイトルの最小文字数。
-}
minTitleLength : Int
minTitleLength =
    1


{-| タイトルの最大文字数。
-}
maxTitleLength : Int
maxTitleLength =
    20


{-| 呼吸法を分類するためのカテゴリの情報を表します。
-}
type alias Category =
    { id : CategoryId
    , title : Title
    }
