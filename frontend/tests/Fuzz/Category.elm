module Fuzz.Category exposing (categoryFuzzer)

{-| カテゴリのFuzzer


### fuzzer

@docs categoryFuzzer

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Common exposing (charFuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Types.Category exposing (Category, CategoryId, maxTitleLength, minTitleLength)


{-| カテゴリのタイトルのFuzzer。
-}
titleFuzzer : Fuzzer String
titleFuzzer =
    Fuzz.listOfLengthBetween minTitleLength maxTitleLength charFuzzer
        |> Fuzz.map String.fromList


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
