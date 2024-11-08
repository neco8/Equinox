module Types.BreathingMethod exposing
    ( PhaseType(..)
    , InhaleDuration, toInhaleDuration, fromInhaleDuration
    , InhaleHoldDuration, toInhaleHoldDuration, fromInhaleHoldDuration
    , ExhaleDuration, toExhaleDuration, fromExhaleDuration
    , ExhaleHoldDuration, toExhaleHoldDuration, fromExhaleHoldDuration
    , BreathingMethodId
    , Name
    , BreathingMethod, toName, fromName, minPhaseDuration, minHoldPhaseDuration, maxPhaseDuration, minNameLength, maxNameLength
    )

{-| このモジュールは、`Inhale`、`InhaleHold`、`Exhale`、`ExhaleHold`のような異なるフェーズから構成される呼吸法を管理するための型と関数を定義します。
各フェーズの時間を設定可能な`BreathingMethod`を定義する構造を提供します。


### 型


#### フェーズを表す型

@docs PhaseType

@docs minPhaseDuration, minHoldPhaseDuration, maxPhaseDuration

@docs InhaleDuration, toInhaleDuration, fromInhaleDuration
@docs InhaleHoldDuration, toInhaleHoldDuration, fromInhaleHoldDuration
@docs ExhaleDuration, toExhaleDuration, fromExhaleDuration
@docs ExhaleHoldDuration, toExhaleHoldDuration, fromExhaleHoldDuration


#### ID

@docs BreathingMethodId


#### 名前

@docs minNameLength, maxNameLength

@docs Name, toName, fromName


#### 呼吸法を表す型

@docs BreathingMethod

-}

import Time exposing (Posix)
import Types.Category exposing (CategoryId)
import Uuid exposing (Uuid)


{-| 呼吸法のフェーズのタイプを表します。各フェーズは、呼吸サイクル内の特定のアクション（吸う、止めるなど）を表します。
-}
type PhaseType
    = Inhale
    | InhaleHold
    | Exhale
    | ExhaleHold


{-| 吸気フェーズの期間（秒単位）。
-}
type InhaleDuration
    = InhaleDuration Int


{-| 吸気フェーズの期間（秒単位）を作成します。
-}
toInhaleDuration : Int -> Maybe InhaleDuration
toInhaleDuration duration =
    if duration >= minPhaseDuration && duration <= maxPhaseDuration then
        Just (InhaleDuration duration)

    else
        Nothing


{-| 吸気フェーズの期間（秒単位）を取得します。
-}
fromInhaleDuration : InhaleDuration -> Int
fromInhaleDuration (InhaleDuration duration) =
    duration


{-| 吸気保持フェーズの期間（秒単位）。
-}
type InhaleHoldDuration
    = InhaleHoldDuration Int


{-| 吸気保持フェーズの期間（秒単位）を作成します。
-}
toInhaleHoldDuration : Int -> Maybe InhaleHoldDuration
toInhaleHoldDuration duration =
    if duration >= minHoldPhaseDuration && duration <= maxPhaseDuration then
        Just (InhaleHoldDuration duration)

    else
        Nothing


{-| 吸気保持フェーズの期間（秒単位）を取得します。
-}
fromInhaleHoldDuration : InhaleHoldDuration -> Int
fromInhaleHoldDuration (InhaleHoldDuration duration) =
    duration


{-| 呼気フェーズの期間（秒単位）。
-}
type ExhaleDuration
    = ExhaleDuration Int


{-| 呼気フェーズの期間（秒単位）を作成します。
-}
toExhaleDuration : Int -> Maybe ExhaleDuration
toExhaleDuration duration =
    if duration >= minPhaseDuration && duration <= maxPhaseDuration then
        Just (ExhaleDuration duration)

    else
        Nothing


{-| 呼気フェーズの期間（秒単位）を取得します。
-}
fromExhaleDuration : ExhaleDuration -> Int
fromExhaleDuration (ExhaleDuration duration) =
    duration


{-| 呼気保持フェーズの期間（秒単位）。
-}
type ExhaleHoldDuration
    = ExhaleHoldDuration Int


{-| 呼気保持フェーズの期間（秒単位）を作成します。
-}
toExhaleHoldDuration : Int -> Maybe ExhaleHoldDuration
toExhaleHoldDuration duration =
    if duration >= minHoldPhaseDuration && duration <= maxPhaseDuration then
        Just (ExhaleHoldDuration duration)

    else
        Nothing


{-| 呼気保持フェーズの期間（秒単位）を取得します。
-}
fromExhaleHoldDuration : ExhaleHoldDuration -> Int
fromExhaleHoldDuration (ExhaleHoldDuration duration) =
    duration


{-| 秒（秒単位）
-}
seconds : Int
seconds =
    1


{-| 分（秒単位）
-}
minutes : Int
minutes =
    60 * seconds


{-| フェーズの最短時間

フェーズの最短秒数は基本的に1秒

-}
minPhaseDuration : Int
minPhaseDuration =
    1 * seconds


{-| フェーズの最短ホールド時間

フェーズの最短ホールド秒数は0秒が許容される

-}
minHoldPhaseDuration : Int
minHoldPhaseDuration =
    0


{-| フェーズの最大時間

フェーズの最大時間は10分

-}
maxPhaseDuration : Int
maxPhaseDuration =
    10 * minutes


{-| 呼吸法を表します。各呼吸法は特定のカテゴリーに属し、各フェーズごとの時間（秒数）を設定します。
-}
type alias BreathingMethod =
    { id : BreathingMethodId
    , name : Name
    , categoryId : CategoryId
    , createdAt : Posix
    , inhaleDuration : InhaleDuration
    , inhaleHoldDuration : InhaleHoldDuration
    , exhaleDuration : ExhaleDuration
    , exhaleHoldDuration : ExhaleHoldDuration
    }


{-| 呼吸法のID（UUID）。
-}
type alias BreathingMethodId =
    Uuid


{-| 呼吸法の名前。
-}
type Name
    = Name String


{-| 呼吸法の名前の最小文字数。
-}
minNameLength : Int
minNameLength =
    1


{-| 呼吸法の名前の最大文字数。
-}
maxNameLength : Int
maxNameLength =
    20


{-| 呼吸法の名前を作成します。
-}
toName : String -> Maybe Name
toName name =
    if String.length name >= minNameLength && String.length name <= maxNameLength then
        Just (Name name)

    else
        Nothing


{-| 呼吸法の名前を取得します。
-}
fromName : Name -> String
fromName (Name name) =
    name
