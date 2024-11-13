module BreathingMethodDurationInput exposing (Config, view)

{-|


## Breathing Method Duration Input

このモジュールは、呼吸法の秒数をセットするためのコンポーネントです。

ManualSessionPreparationPageとBreathingMethodAddPageの間で共通化を図ります。

-}

import Html exposing (Html, div, input)
import Html.Attributes exposing (attribute, value)
import Html.Events exposing (onInput)


{-| BreathingMethodDurationInputを使用する際に必要な設定を定義します。
-}
type alias Config msg =
    { onInputInhaleDuration : String -> msg
    , onInputInhaleHoldDuration : String -> msg
    , onInputExhaleDuration : String -> msg
    , onInputExhaleHoldDuration : String -> msg
    }


{-| BreathingMethodDurationInputのビューを生成します。
-}
view : Config msg -> { model | inhaleDurationInput : String, inhaleHoldDurationInput : String, exhaleDurationInput : String, exhaleHoldDurationInput : String } -> Html msg
view config model =
    div []
        [ input
            [ attribute "aria-label" "inhale-duration-input"
            , onInput config.onInputInhaleDuration
            , value model.inhaleDurationInput
            ]
            []
        , input
            [ attribute "aria-label" "inhale-hold-duration-input"
            , onInput config.onInputInhaleHoldDuration
            , value model.inhaleHoldDurationInput
            ]
            []
        , input
            [ attribute "aria-label" "exhale-duration-input"
            , onInput config.onInputExhaleDuration
            , value model.exhaleDurationInput
            ]
            []
        , input
            [ attribute "aria-label" "exhale-hold-duration-input"
            , onInput config.onInputExhaleHoldDuration
            , value model.exhaleHoldDurationInput
            ]
            []
        ]
