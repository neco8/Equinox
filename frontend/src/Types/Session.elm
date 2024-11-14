module Types.Session exposing
    ( SessionId
    , Duration, toDuration, fromDuration, minSessionDuration, maxSessionDuration
    , Session
    )

{-|


## Session

このモジュールは、呼吸法セッションを表す型と関連する関数を定義します。

セッションは、特定の呼吸法 (BreathingMethod) に基づいており、個別のフェーズごとの時間 (Inhale, InhaleHold, Exhale, ExhaleHold) を持つことで、ユーザーの呼吸セッションの進捗を記録します。また、呼吸法IDを持つことで、特定の呼吸法との関連を表現します。

セッションはいつまでも記録されます。呼吸法が削除されたとしても、そのセッションは残ります。


### 型

@docs SessionId
@docs Duration, toDuration, fromDuration, minSessionDuration, maxSessionDuration
@docs Session

-}

import Time exposing (Posix)
import Types.BreathingMethod exposing (BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name)
import Uuid exposing (Uuid)


{-| セッションのID (UUID)。
-}
type alias SessionId =
    Uuid


{-| セッション全体の合計時間（秒単位）。計算されます。
-}
type Duration
    = Duration Int


{-| 分（秒単位）。
-}
minutes : Int
minutes =
    60


{-| 時間（秒単位）。
-}
hours : Int
hours =
    60 * minutes


{-| セッションの最短時間（秒単位）。

最短時間は1分となる。

-}
minSessionDuration : Int
minSessionDuration =
    1 * minutes


{-| セッションの最長時間（秒単位）。

最長時間は10時間となる。

-}
maxSessionDuration : Int
maxSessionDuration =
    10 * hours


{-| Duration型を作成する
-}
toDuration : Int -> Maybe Duration
toDuration duration =
    if duration >= minSessionDuration && duration <= maxSessionDuration then
        Just (Duration duration)

    else
        Nothing


{-| Durationを取得する
-}
fromDuration : Duration -> Int
fromDuration (Duration duration) =
    duration


{-| 呼吸法のセッションを表す型。各セッションは個別のフェーズごとの期間を設定し、特定の呼吸法に紐づいています。

  - `breathingMethodId` を通じて、特定の `BreathingMethod` と関連付けられますが、カスタムセッションを許容するために、将来的にオプション化も検討されている。
  - 各フェーズ（吸気、吸気保持、呼気、呼気保持）の期間を保持し、ユーザーのセッションデータとして保存されます。

-}
type alias Session =
    { id : SessionId
    , inhaleDuration : InhaleDuration
    , inhaleHoldDuration : InhaleHoldDuration
    , exhaleDuration : ExhaleDuration
    , exhaleHoldDuration : ExhaleHoldDuration
    , breathingMethodId : Maybe BreathingMethodId
    , breathingMethodName : Maybe Name
    , duration : Duration
    , createdAt : Posix
    }
