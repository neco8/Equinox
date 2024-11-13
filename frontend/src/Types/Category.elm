module Types.Category exposing
    ( CategoryId
    , Title, toTitle, fromTitle, minTitleLength, maxTitleLength
    , Category
    )

{-|


## Category

このモジュールは、呼吸法に関連する`Category`の型と関数を定義します。

`Category`にはカテゴリID、タイトルなどが含まれます。

呼吸法は特定のカテゴリに関連付けられることができ、カテゴリごとに呼吸法を分類するために使用されます。


### 型

@docs CategoryId
@docs Title, toTitle, fromTitle, minTitleLength, maxTitleLength
@docs Category

-}

import Uuid exposing (Uuid)


{-| カテゴリーのID（UUID）。
-}
type alias CategoryId =
    Uuid


{-| カテゴリのタイトル。
-}
type Title
    = Title String


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


{-| カテゴリのタイトルを作成する
-}
toTitle : String -> Maybe Title
toTitle title =
    if String.length title >= minTitleLength && String.length title <= maxTitleLength then
        Just (Title title)

    else
        Nothing


{-| カテゴリのタイトルを取得する
-}
fromTitle : Title -> String
fromTitle (Title title) =
    title


{-| 呼吸法を分類するためのカテゴリの情報を表します。
-}
type alias Category =
    { id : CategoryId
    , title : Title
    }
