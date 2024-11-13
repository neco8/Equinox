module Fuzz.Session exposing (sessionFuzzer)

{-|


## Session Fuzzer

このモジュールは、セッションのFuzzerを提供します。セッションは呼吸法と紐づいていますが、呼吸法が削除されている可能性もあります。

frequencyとしては、95%の確率で呼吸法リストから選択し、5%の確率で削除された呼吸法を選択します。そのような重み付けで、削除された呼吸法が選択される確率を低くしています。


### fuzzer

@docs sessionFuzzer

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Time
import Types.BreathingMethod exposing (BreathingMethod)
import Types.Session exposing (Duration, Session, maxSessionDuration, minSessionDuration, toDuration)
import Uuid exposing (Uuid)


{-| セッションのFuzzer。
-}
sessionFuzzer : ( BreathingMethod, List BreathingMethod ) -> BreathingMethod -> Fuzzer Time.Posix -> Fuzzer Session
sessionFuzzer breathingMethods deletedBreathingMethod =
    let
        associatedBreathingMethodFuzzer : Fuzzer BreathingMethod
        associatedBreathingMethodFuzzer =
            case breathingMethods of
                ( head, tail ) ->
                    Fuzz.frequency
                        [ ( 95, Fuzz.oneOfValues (head :: tail) )
                        , ( 5, Fuzz.constant deletedBreathingMethod )
                        ]

        durationFuzzer : () -> Fuzzer Duration
        durationFuzzer _ =
            Fuzz.intRange minSessionDuration maxSessionDuration
                |> Fuzz.andThen
                    (\i ->
                        case toDuration i of
                            Just d ->
                                Fuzz.constant d

                            Nothing ->
                                durationFuzzer ()
                    )

        createSession : Uuid -> BreathingMethod -> Duration -> Time.Posix -> Session
        createSession id bm =
            Session
                id
                bm.inhaleDuration
                bm.inhaleHoldDuration
                bm.exhaleDuration
                bm.exhaleHoldDuration
                bm.id
                bm.name
    in
    Fuzz.map4
        createSession
        uuidFuzzer
        associatedBreathingMethodFuzzer
        (durationFuzzer ())
