module Fuzz.BreathingMethod exposing (..)

import Fuzz exposing (Fuzzer)
import Fuzz.Common exposing (charFuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Time
import Types.BreathingMethod exposing (BreathingMethod, PhaseType(..), maxNameLength, maxPhaseDuration, minHoldPhaseDuration, minNameLength, minPhaseDuration)
import Types.Category exposing (Category, CategoryId)


{-| 名前のFuzzer。名前の最小・最大長を満たし、ランダムな文字列を生成。
-}
nameFuzzer : Fuzzer String
nameFuzzer =
    Fuzz.listOfLengthBetween minNameLength maxNameLength charFuzzer
        |> Fuzz.map String.fromList


{-| `BreathingMethod` 型のFuzzer。
カテゴリーや時間などの各フィールドをランダムに生成。

    受け取るパラメータ:
    - `(Category, List Category)`: 一つ以上のカテゴリー情報。リストの先頭要素とその他の要素に分かれる。
    - `Time.Posix`: 現在の基準時刻。

    返却:
    - `Fuzzer BreathingMethod`: ランダムに生成される `BreathingMethod` のFuzzer。

-}
breathingMethodFuzzer : ( Category, List Category ) -> Time.Posix -> Fuzzer BreathingMethod
breathingMethodFuzzer categories referenceTime =
    let
        phaseDurationFuzzer : PhaseType -> Fuzzer Int
        phaseDurationFuzzer phaseType =
            let
                minDuration =
                    case phaseType of
                        InhaleHold ->
                            minHoldPhaseDuration

                        ExhaleHold ->
                            minHoldPhaseDuration

                        _ ->
                            minPhaseDuration
            in
            Fuzz.intRange minDuration maxPhaseDuration

        categoryIdFuzzer : Fuzzer CategoryId
        categoryIdFuzzer =
            let
                ( head, tail ) =
                    categories
            in
            Fuzz.oneOfValues (List.map .id (head :: tail))

        createdAtFuzzer : Fuzzer Time.Posix
        createdAtFuzzer =
            let
                now =
                    Time.posixToMillis referenceTime

                yearInMillis =
                    365 * 24 * 60 * 60 * 1000
            in
            Fuzz.map
                (\offset ->
                    Time.millisToPosix (now - offset)
                )
                (Fuzz.intRange 0 yearInMillis)
    in
    Fuzz.map8
        BreathingMethod
        uuidFuzzer
        nameFuzzer
        categoryIdFuzzer
        createdAtFuzzer
        (phaseDurationFuzzer Inhale)
        (phaseDurationFuzzer InhaleHold)
        (phaseDurationFuzzer Exhale)
        (phaseDurationFuzzer ExhaleHold)
