module Fuzz.Category exposing (categoryFuzzer)

{-|


## Category Fuzzer

このモジュールは、カテゴリのFuzzerを提供します。

カテゴリは共通のFuzzerである`charFuzzer`を利用しています。ですので、多様な文字列を利用したカテゴリがテストできます。


### fuzzer

@docs categoryFuzzer

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Common exposing (charFuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Types.Category exposing (Category, CategoryId, Title, maxTitleLength, minTitleLength, toTitle)


{-| カテゴリのタイトルのFuzzer。
-}
titleFuzzer : Fuzzer Title
titleFuzzer =
    Fuzz.listOfLengthBetween minTitleLength maxTitleLength charFuzzer
        |> Fuzz.map String.fromList
        |> Fuzz.andThen
            (\s ->
                case toTitle s of
                    Just t ->
                        Fuzz.constant t

                    Nothing ->
                        titleFuzzer
            )


{-| カテゴリIDのFuzzer。
-}
categoryIdFuzzer : Fuzzer CategoryId
categoryIdFuzzer =
    uuidFuzzer


{-| カテゴリのFuzzer。
-}
categoryFuzzer : Fuzzer Category
categoryFuzzer =
    Fuzz.map2 Category categoryIdFuzzer titleFuzzer
