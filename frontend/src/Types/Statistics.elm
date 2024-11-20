module Types.Statistics exposing
    ( Statistics
    , recentDaysThreshold
    , calculateFromSessions, calculateRecentFromSessions, calculateStreak
    )

{-|


## Statistics

このモジュールは、セッションのリストから統計情報を生成するための型および関数を提供します。

このモジュールは、特に以下の機能を提供します：

  - `calculateFromSessions` 関数は、全体のセッションリストを基に統計情報（合計セット数、合計秒数、練習日数）を計算します。
  - `calculateRecentFromSessions` 関数は、指定された日数以内の最近のセッションをフィルタリングし、それに基づいて統計情報を生成します。
  - `recentDaysThreshold` 定数は、最近の日数のデフォルトしきい値（7日）を定義しており、`calculateRecentFromSessions` のデフォルト設定として使用されます。

このモジュールの主な用途は、セッションデータからの総合的な統計と、短期間に限定した統計の取得です。


### 型

@docs Statistics


### 定数

@docs recentDaysThreshold


### 関数

@docs calculateFromSessions, calculateRecentFromSessions, calculateStreak


### TODO

  - [ ] TODO: タイムゾーンを追加する案件にて、タイムゾーンをそれぞれの場所に合わせるように実装する

-}

import Date exposing (Interval(..), Unit(..), format)
import List.Extra
import Time exposing (posixToMillis)
import Types.Session exposing (Session, fromDuration)


{-| `Statistics` 型はセッションに基づいた統計情報を表します。

  - `totalSets` は総セット数を示します。
  - `totalSeconds` はセッションで消費した合計時間（秒単位）です。
  - `totalPracticeDays` は異なる練習日の合計数を表します。

-}
type alias Statistics =
    { totalSets : Int
    , totalSeconds : Int
    , totalPracticeDays : Int
    }


{-| `recentDaysThreshold` は最近の日数のしきい値を表し、デフォルトで7日間に設定されています。
-}
recentDaysThreshold : Int
recentDaysThreshold =
    7


{-| `calculateFromSessions` は与えられたセッションのリストに基づいて統計情報を計算します。

  - [ ] TODO: タイムゾーン追加案件にて、タイムゾーンをそれぞれの場所に合わせるように実装。

-}
calculateFromSessions : List Session -> Statistics
calculateFromSessions sessions =
    { totalSets = List.length sessions
    , totalSeconds =
        List.foldr (\session acc -> acc + fromDuration session.duration)
            0
            sessions
    , totalPracticeDays =
        sessions
            |> List.map
                (.createdAt
                    >> Date.fromPosix Time.utc
                )
            -- [ ] TODO: タイムゾーン追加案件にて、タイムゾーンをそれぞれの場所に合わせるように実装
            |> List.Extra.uniqueBy
                (format "yyyy-MM-dd")
            |> List.length
    }


{-| `calculateRecentFromSessions` は指定された `nDays` 以内に作成されたセッションから統計を計算します。

  - `nDays` は最近の日数のしきい値を表します。`nDays` 日以内に作成されたセッションのみが対象となります。
  - `currentTime` は基準となる現在の時刻（Posix型）です。

最近のセッションをフィルタリングし、`calculateFromSessions` を使用して統計を生成します。しきい値には、当モジュールの `recentDaysThreshold` 定数を利用してください。

-}
calculateRecentFromSessions : Int -> Time.Posix -> List Session -> Statistics
calculateRecentFromSessions nDays currentTime sessions =
    let
        currentTimeMs =
            posixToMillis currentTime

        nDaysInMs =
            nDays * 24 * 60 * 60 * 1000

        cutoffTime =
            currentTimeMs - nDaysInMs

        isRecent session =
            posixToMillis session.createdAt
                > cutoffTime
                && posixToMillis session.createdAt
                <= currentTimeMs

        recentSessions =
            List.filter isRecent sessions
    in
    calculateFromSessions recentSessions


{-| `calculateStreak` はセッションのリストから連続した日数を計算します。

  - `calculateStreak` は、セッションのリストを受け取り、連続した日数を返します。
  - 連続した日数は、セッションの作成日時を基に計算されます。

-}
calculateStreak : List Session -> Int
calculateStreak =
    List.map (.createdAt >> Date.fromPosix Time.utc >> Date.toRataDie)
        >> List.Extra.unique
        >> List.sort
        >> List.reverse
        >> List.Extra.groupWhile
            (\a b ->
                a - b <= 1
            )
        >> (<|) (List.head >> Maybe.map (curry (::) >> List.length) >> Maybe.withDefault 0)


{-| `curry` 関数は、関数 `f` をカリー化します。

  - `curry` は、関数 `f` とタプル `(a, b)` を受け取り、`f` をカリー化して `(a, b)` に適用します。

-}
curry : (a -> b -> c) -> ( a, b ) -> c
curry f ( a, b ) =
    f a b
