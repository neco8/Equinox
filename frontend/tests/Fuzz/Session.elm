module Fuzz.Session exposing (sessionFuzzer)

{-| セッションのFuzzer


### fuzzer

@docs sessionFuzzer

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Uuid exposing (uuidFuzzer)
import Time
import Types.BreathingMethod exposing (BreathingMethod)
import Types.Session exposing (Session, maxSessionDuration, minSessionDuration)
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

        durationFuzzer : Fuzzer Int
        durationFuzzer =
            Fuzz.intRange minSessionDuration maxSessionDuration

        createSession : Uuid -> BreathingMethod -> Int -> Time.Posix -> Session
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
        durationFuzzer
