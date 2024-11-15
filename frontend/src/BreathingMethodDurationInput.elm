module BreathingMethodDurationInput exposing (Config, view)

{-|


## Breathing Method Duration Input

このモジュールは、呼吸法の秒数をセットするためのコンポーネントです。

ManualSessionPreparationPageとBreathingMethodAddPageの間で共通化を図ります。

-}

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (attribute, class, value)
import Html.Events exposing (onInput)
import Icon
import Types.BreathingMethod exposing (PhaseType(..))
import Html.Attributes exposing (type_)
import Html.Attributes exposing (placeholder)


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
    let
        verbosePhaseType phaseType =
            case phaseType of
                Inhale ->
                    "吸う"

                InhaleHold ->
                    "止める"

                Exhale ->
                    "吐く"

                ExhaleHold ->
                    "止める"

        toValue phaseType =
            case phaseType of
                Inhale ->
                    model.inhaleDurationInput

                InhaleHold ->
                    model.inhaleHoldDurationInput

                Exhale ->
                    model.exhaleDurationInput

                ExhaleHold ->
                    model.exhaleHoldDurationInput

        toColorClass phaseType =
            case phaseType of
                Inhale ->
                    { textColorClass = class "text-blue-600"
                    , bgColorClass = class "bg-blue-50"
                    , borderColorClass = class "border-blue-200"
                    , ringColorClass = class "ring-blue-300"
                    }

                InhaleHold ->
                    { textColorClass = class "text-indigo-600"
                    , bgColorClass = class "bg-indigo-50"
                    , borderColorClass = class "border-indigo-200"
                    , ringColorClass = class "ring-indigo-300"
                    }

                Exhale ->
                    { textColorClass = class "text-purple-600"
                    , bgColorClass = class "bg-purple-50"
                    , borderColorClass = class "border-purple-200"
                    , ringColorClass = class "ring-purple-300"
                    }

                ExhaleHold ->
                    { textColorClass = class "text-pink-600"
                    , bgColorClass = class "bg-pink-50"
                    , borderColorClass = class "border-pink-200"
                    , ringColorClass = class "ring-pink-300"
                    }

        phaseTypeToString phaseType =
            case phaseType of
                Inhale ->
                    "inhale"

                InhaleHold ->
                    "inhale-hold"

                Exhale ->
                    "exhale"

                ExhaleHold ->
                    "exhale-hold"

        toIcon phaseType =
            case phaseType of
                Inhale ->
                    Icon.view Icon.Wind

                InhaleHold ->
                    Icon.view Icon.Pause

                Exhale ->
                    div [ class "transform-rotate-180" ] [ Icon.view Icon.Wind ]

                ExhaleHold ->
                    Icon.view Icon.Pause

        toAriaLabel phaseType =
            attribute "aria-label" <| (phaseTypeToString phaseType ++ "-duration-input")

        toOnInput phaseType =
            case phaseType of
                Inhale ->
                    config.onInputInhaleDuration

                InhaleHold ->
                    config.onInputInhaleHoldDuration

                Exhale ->
                    config.onInputExhaleDuration

                ExhaleHold ->
                    config.onInputExhaleHoldDuration
    in
    div [ class "grid grid-cols-2 gap-4 mb-8" ] <|
        List.map
            (\phaseType ->
                div
                    [ class "flex flex-col items-center p-4 rounded-lg"
                    , (toColorClass phaseType).bgColorClass
                    ]
                    [ div [ class "flex items-center space-x-2 mb-2" ]
                        [ toIcon phaseType
                        , span [ class "text-sm text-gray-600" ] [ text <| verbosePhaseType phaseType ]
                        ]
                    , input
                        [ toAriaLabel phaseType
                        , onInput (toOnInput phaseType)
                        , value (toValue phaseType)
                        , class "text-center rounded-lg border bg-transparent focus:ring-4 text-2xl font-semibold"
                        , (toColorClass phaseType).textColorClass
                        , (toColorClass phaseType).borderColorClass
                        , (toColorClass phaseType).ringColorClass
                        , placeholder "0"
                        ]
                        []
                    , span [ class "text-xs text-gray-500 mt-1" ] [ text "秒" ]
                    ]
            )
            [ Inhale, InhaleHold, Exhale, ExhaleHold ]
