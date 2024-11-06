module Fuzz.Uuid exposing (uuidFuzzer)

{-| UUIDのFuzzer

このモジュールは、UUIDのFuzzerを提供します。UUID version 4の形式に従う文字列を生成します。


### fuzzer

@docs uuidFuzzer

-}

import Fuzz exposing (Fuzzer)
import Uuid exposing (Uuid)


{-| 16進数の文字を生成するFuzzer
-}
hexDigitFuzzer : Fuzzer Char
hexDigitFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant '0'
        , Fuzz.constant '1'
        , Fuzz.constant '2'
        , Fuzz.constant '3'
        , Fuzz.constant '4'
        , Fuzz.constant '5'
        , Fuzz.constant '6'
        , Fuzz.constant '7'
        , Fuzz.constant '8'
        , Fuzz.constant '9'
        , Fuzz.constant 'a'
        , Fuzz.constant 'b'
        , Fuzz.constant 'c'
        , Fuzz.constant 'd'
        , Fuzz.constant 'e'
        , Fuzz.constant 'f'
        ]


{-| N桁の16進数を生成するFuzzer
-}
hexStringFuzzer : Int -> Fuzzer String
hexStringFuzzer n =
    Fuzz.listOfLength n hexDigitFuzzer
        |> Fuzz.map String.fromList


{-| UUID version 4の中央部分のFuzzer
-}
midPartFuzzer : Fuzzer String
midPartFuzzer =
    Fuzz.map (\s -> "4" ++ s)
        (hexStringFuzzer 3)


{-| UUID version 4のvariantビットを生成するFuzzer
-}
variantBitFuzzer : Fuzzer Char
variantBitFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant '8'
        , Fuzz.constant '9'
        , Fuzz.constant 'a'
        , Fuzz.constant 'b'
        ]


{-| UUID version 4のvariantビットを含む部分のFuzzer
-}
variantPartFuzzer : Fuzzer String
variantPartFuzzer =
    Fuzz.map2 (\v rest -> String.fromChar v ++ rest)
        variantBitFuzzer
        (hexStringFuzzer 3)


{-| UUIDのFuzzer
-}
uuidFuzzer : Fuzzer Uuid
uuidFuzzer =
    Fuzz.map5
        (\part1 part2 part3 part4 part5 ->
            String.join "-"
                [ part1
                , part2
                , part3
                , part4
                , part5
                ]
        )
        (hexStringFuzzer 8)
        (hexStringFuzzer 4)
        midPartFuzzer
        variantPartFuzzer
        (hexStringFuzzer 12)
        |> Fuzz.andThen
            (\s ->
                case Uuid.fromString s of
                    Just uuid ->
                        Fuzz.constant uuid

                    Nothing ->
                        uuidFuzzer
            )
