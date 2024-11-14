module Fuzz.Common exposing (..)

{-|


## Common Fuzzer

このモジュールは、共通のFuzzerを提供します。


### Char

charは、ASCII printable characters (0x20-0x7E) と、ひらがな、カタカナ、漢字を含む文字を生成するFuzzerです。

@docs charFuzzer


### NonEmptyList

NonEmptyListは、一つ以上のListを表す型です。

@docs NonEmptyList, nonEmptyListFuzzer

-}

import Fuzz exposing (Fuzzer)


{-| ASCII printable characters (0x20-0x7E) と、ひらがな、カタカナ、漢字を含む文字のFuzzer。
-}
charFuzzer : Fuzzer Char
charFuzzer =
    Fuzz.oneOf
        [ -- ASCII printable characters (0x20-0x7E)
          Fuzz.intRange 0x20 0x7E
            |> Fuzz.map Char.fromCode
        , -- ひらがな (0x3040-0x309F)
          Fuzz.intRange 0x3040 0x309F
            |> Fuzz.map Char.fromCode
        , -- カタカナ (0x30A0-0x30FF)
          Fuzz.intRange 0x30A0 0x30FF
            |> Fuzz.map Char.fromCode
        , -- 漢字 (0x4E00-0x9FFF)
          Fuzz.intRange 0x4E00 0x9FFF
            |> Fuzz.map Char.fromCode
        ]


{-| 一つ以上のListを表す型。
-}
type alias NonEmptyList a =
    ( a, List a )


{-| 最小長と最大長を指定して、空でないリストのFuzzerを生成。

最大長が1未満の場合、最大長は1に設定されます。

-}
nonEmptyListFuzzer : Int -> Int -> Fuzzer a -> Fuzzer (NonEmptyList a)
nonEmptyListFuzzer minLength maxLength itemFuzzer =
    Fuzz.listOfLengthBetween minLength (max 1 maxLength) itemFuzzer
        |> Fuzz.andThen
            (\items ->
                case items of
                    [] ->
                        nonEmptyListFuzzer minLength maxLength itemFuzzer

                    head :: tail ->
                        Fuzz.constant ( head, tail )
            )
