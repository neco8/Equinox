module Fuzz.BreathingMethod exposing (breathingMethodFuzzer)

{-|


## BreathingMethod Fuzzer

このモジュールは、呼吸法のFuzzerを提供します。呼吸法はカテゴリーに紐づいています。


### fuzzer

基準時刻に基づいて、1年以内の過去の日時に紐づいた呼吸法を生成します。

呼吸法の作成日時をカスタマイズすることには対応していません。

@docs breathingMethodFuzzer

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Common exposing (charFuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Time
import Types.BreathingMethod exposing (BreathingMethod, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name, PhaseType(..), maxNameLength, maxPhaseDuration, minHoldPhaseDuration, minNameLength, minPhaseDuration, toExhaleDuration, toExhaleHoldDuration, toInhaleDuration, toInhaleHoldDuration, toName)
import Types.Category exposing (Category, CategoryId)


{-| 名前のFuzzer。名前の最小・最大長を満たし、ランダムな文字列を生成。
-}
nameFuzzer : Fuzzer Name
nameFuzzer =
    Fuzz.listOfLengthBetween minNameLength maxNameLength charFuzzer
        |> Fuzz.map String.fromList
        |> Fuzz.andThen
            (\s ->
                case toName s of
                    Just n ->
                        Fuzz.constant n

                    Nothing ->
                        nameFuzzer
            )


{-| `BreathingMethod` 型のFuzzer。
カテゴリーや時間などの各フィールドをランダムに生成。

    受け取るパラメータ:
    - `(Category, List Category)`: 一つ以上のカテゴリー情報。リストの先頭要素とその他の要素に分かれる。
    - `Time.Posix`: 基準時刻。

    返却:
    - `Fuzzer BreathingMethod`: ランダムに生成される `BreathingMethod` のFuzzer。

-}
breathingMethodFuzzer : ( Category, List Category ) -> Time.Posix -> Fuzzer BreathingMethod
breathingMethodFuzzer categories referenceTime =
    let
        inhaleDurationFuzzer : () -> Fuzzer InhaleDuration
        inhaleDurationFuzzer _ =
            Fuzz.intRange minPhaseDuration maxPhaseDuration
                |> Fuzz.map toInhaleDuration
                |> Fuzz.andThen
                    (\id ->
                        case id of
                            Just i ->
                                Fuzz.constant i

                            Nothing ->
                                inhaleDurationFuzzer ()
                    )

        inhaleHoldDurationFuzzer : () -> Fuzzer InhaleHoldDuration
        inhaleHoldDurationFuzzer _ =
            Fuzz.intRange minHoldPhaseDuration maxPhaseDuration
                |> Fuzz.map toInhaleHoldDuration
                |> Fuzz.andThen
                    (\ihd ->
                        case ihd of
                            Just ih ->
                                Fuzz.constant ih

                            Nothing ->
                                inhaleHoldDurationFuzzer ()
                    )

        exhaleDurationFuzzer : () -> Fuzzer ExhaleDuration
        exhaleDurationFuzzer _ =
            Fuzz.intRange minPhaseDuration maxPhaseDuration
                |> Fuzz.map toExhaleDuration
                |> Fuzz.andThen
                    (\ed ->
                        case ed of
                            Just e ->
                                Fuzz.constant e

                            Nothing ->
                                exhaleDurationFuzzer ()
                    )

        exhaleHoldDurationFuzzer : () -> Fuzzer ExhaleHoldDuration
        exhaleHoldDurationFuzzer _ =
            Fuzz.intRange minHoldPhaseDuration maxPhaseDuration
                |> Fuzz.map toExhaleHoldDuration
                |> Fuzz.andThen
                    (\ehd ->
                        case ehd of
                            Just eh ->
                                Fuzz.constant eh

                            Nothing ->
                                exhaleHoldDurationFuzzer ()
                    )

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
        (inhaleDurationFuzzer ())
        (inhaleHoldDurationFuzzer ())
        (exhaleDurationFuzzer ())
        (exhaleHoldDurationFuzzer ())
