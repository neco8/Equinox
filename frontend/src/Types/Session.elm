module Types.Session exposing (..)

{-| このモジュールは、呼吸法セッションを表す型と関連する関数を定義します。
セッションは、特定の呼吸法 (BreathingMethod) に基づいており、個別のフェーズごとの時間 (Inhale, InhaleHold, Exhale, ExhaleHold) を持つことで、ユーザーの呼吸セッションの進捗を記録します。また、呼吸法IDを持つことで、特定の呼吸法との関連を表現します。


### 型

@docs SessionId, Duration, Session


### TODO

  - [ ] TODO: 必要に応じて、BreathingMethodに紐づかないカスタムセッションの対応を検討し、保存形式の変更を考慮する。

-}

import Time exposing (Posix)
import Types.BreathingMethod exposing (BreathingMethodId, ExhaleDuration, ExhaleHoldDuration, InhaleDuration, InhaleHoldDuration, Name)
import Uuid exposing (Uuid)


{-| セッションのID (UUID)。
-}
type alias SessionId =
    Uuid


{-| セッション全体の合計時間（秒単位）。計算されます。

  - [ ] TODO: Opaque Typeに変更する。最短時間と最長時間が存在するため。

-}
type alias Duration =
    Int


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
    , breathingMethodId : BreathingMethodId
    , breathingMethodName : Name
    , duration : Duration
    , createdAt : Posix
    }
