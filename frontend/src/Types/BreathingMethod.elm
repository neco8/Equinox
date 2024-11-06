module Types.BreathingMethod exposing (..)

{-| このモジュールは、`Inhale`、`InhaleHold`、`Exhale`、`ExhaleHold`のような異なるフェーズから構成される呼吸法を管理するための型と関数を定義します。
各フェーズの時間を設定可能な`BreathingMethod`を定義する構造を提供します。


### 型

@docs PhaseType, InhaleDuration, InhaleHoldDuration, ExhaleDuration, ExhaleHoldDuration, BreathingMethod, BreathingMethodId


### TODO

  - [ ] TODO: `InhalerDuration`、`InhaleHoldDuration`、`ExhaleDuration`、`ExhaleHoldDuration`をOpaque Typeに変更し、操作関数のみを提供する。

-}

import Types.Category exposing (CategoryId)
import Time exposing (Posix)
import Uuid exposing (Uuid)


{-| 呼吸法のフェーズのタイプを表します。各フェーズは、呼吸サイクル内の特定のアクション（吸う、止めるなど）を表します。
-}
type PhaseType
    = Inhale
    | InhaleHold
    | Exhale
    | ExhaleHold


{-| 吸気フェーズの期間（秒単位）。

  - [ ] TODO: Opaque Typeに変更する。

-}
type alias InhaleDuration =
    Int


{-| 吸気保持フェーズの期間（秒単位）。

  - [ ] TODO: Opaque Typeに変更する。

-}
type alias InhaleHoldDuration =
    Int


{-| 吐気フェーズの期間（秒単位）。

  - [ ] TODO: Opaque Typeに変更する。

-}
type alias ExhaleDuration =
    Int


{-| 吐気保持フェーズの期間（秒単位）。

  - [ ] TODO: Opaque Typeに変更する。

-}
type alias ExhaleHoldDuration =
    Int


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

  - [ ] TODO: Opaque Typeに変更する。

-}
type alias Name =
    String
