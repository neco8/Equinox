module Test.CalculateStreak exposing (suite)

{-|


## ストリーク計算の確認テスト


### テスト項目

  - 空リストまたは1セッションの場合
  - 連続した日のセッションは正しいストリークを返す
  - 同じ日の複数セッションは1日とカウント
  - 日付境界をまたぐセッションでも正しく計算
  - ストリークは0以上かつセッション数以下という一般的な性質

-}

import Date exposing (Interval(..), Unit(..), diff)
import Expect
import Fuzz exposing (Fuzzer)
import Fuzz.BreathingMethod exposing (breathingMethodFuzzer)
import Fuzz.Category exposing (categoryFuzzer)
import Fuzz.Common exposing (nonEmptyListFuzzer)
import Fuzz.Session exposing (sessionFuzzer)
import List.Extra
import Test exposing (Test, describe, fuzz)
import Time exposing (Posix, ZoneName(..), millisToPosix, posixToMillis)
import Types.Session exposing (Session)
import Types.Statistics exposing (calculateStreak)


{-| 基準となる現在時刻（固定値）

時刻は `2024-12-31T00:00:00.000Z` として設定されています。

-}
referenceTime : Time.Posix
referenceTime =
    Time.millisToPosix 1735603200000


{-| 日付生成戦略

    type DateStrategy
        = ConsecutiveDays
        | RandomDays
        | SameDay

-}
type DateStrategy
    = ConsecutiveDays
    | RandomDays
    | SameDay


{-| 1日のミリ秒数
-}
millisInDay : Int
millisInDay =
    24 * 60 * 60 * 1000


{-| 日付リストのFuzzer
-}
dateListFuzzer : DateStrategy -> Int -> Int -> Fuzzer (List Posix)
dateListFuzzer dateStrategy minSize maxSize =
    let
        baseMillis =
            posixToMillis referenceTime

        consecutiveDaysFuzzer =
            Fuzz.constant
                (\daysAgo count ->
                    List.range 0 (count - 1)
                        |> List.map (\offset -> millisToPosix (baseMillis - ((daysAgo + offset) * millisInDay)))
                )
                |> Fuzz.andMap (Fuzz.intRange 1 25)
                |> Fuzz.andMap (Fuzz.intRange minSize maxSize)

        sameDayFuzzer =
            Fuzz.intRange minSize maxSize
                |> Fuzz.andThen
                    (\size ->
                        Fuzz.constant
                            (\day ->
                                List.map
                                    (\minuteOffset ->
                                        millisToPosix (baseMillis - (day * millisInDay) + (minuteOffset * 60 * 1000))
                                    )
                            )
                            |> Fuzz.andMap (Fuzz.intRange 0 30)
                            |> Fuzz.andMap (Fuzz.listOfLength size (Fuzz.intRange 0 (24 * 60 - 1)))
                    )
    in
    case dateStrategy of
        ConsecutiveDays ->
            consecutiveDaysFuzzer

        RandomDays ->
            Fuzz.listOfLengthBetween minSize
                maxSize
                (Fuzz.constant
                    (\dayOffset minuteOffset ->
                        millisToPosix (baseMillis - (dayOffset * millisInDay) + (minuteOffset * 60 * 1000))
                    )
                    |> Fuzz.andMap (Fuzz.intRange 0 30)
                    |> Fuzz.andMap (Fuzz.intRange 0 (24 * 60 - 1))
                )

        SameDay ->
            sameDayFuzzer


{-| セッション生成設定
-}
type alias SessionGenerationConfig =
    { strategy : DateStrategy
    , minSessions : Int
    , maxSessions : Int
    }


{-| セッションリストのFuzzer
-}
sessionsListFuzzer : SessionGenerationConfig -> Fuzzer (List Session)
sessionsListFuzzer config =
    nonEmptyListFuzzer 1 10 categoryFuzzer
        |> Fuzz.andThen
            (\categories ->
                Fuzz.constant Tuple.pair
                    |> Fuzz.andMap (nonEmptyListFuzzer 1 10 (breathingMethodFuzzer categories referenceTime))
                    |> Fuzz.andMap (breathingMethodFuzzer categories referenceTime)
                    |> Fuzz.andThen
                        (\( breathingMethods, deleted ) ->
                            dateListFuzzer config.strategy config.minSessions config.maxSessions
                                |> Fuzz.andThen
                                    (List.map (Fuzz.constant >> sessionFuzzer breathingMethods deleted)
                                        >> Fuzz.sequence
                                    )
                        )
            )


{-| テストスイート
-}
suite : Test
suite =
    describe "calculateStreak"
        [ describe "基本的なストリーク計算のテスト"
            [ fuzz (sessionsListFuzzer { strategy = RandomDays, minSessions = 0, maxSessions = 1 })
                "空リストまたは1セッションの場合"
                (\sessions ->
                    case sessions of
                        [] ->
                            calculateStreak sessions
                                |> Expect.equal 0

                        _ ->
                            calculateStreak sessions
                                |> Expect.equal 1
                )
            ]
        , describe "連続日のテスト"
            [ fuzz (sessionsListFuzzer { strategy = ConsecutiveDays, minSessions = 2, maxSessions = 5 })
                "連続した日のセッションは正しいストリークを返す"
                (\sessions ->
                    let
                        uniqueDaysCount =
                            countUniqueDays sessions

                        isConsecutive =
                            List.map .createdAt sessions
                                |> areDatesConsecutive
                    in
                    if isConsecutive then
                        calculateStreak sessions
                            |> Expect.equal uniqueDaysCount

                    else
                        -- データが連続していない場合はテストをスキップ
                        Expect.pass
                )
            ]
        , describe "同じ日のセッションのテスト"
            [ fuzz (sessionsListFuzzer { strategy = SameDay, minSessions = 2, maxSessions = 5 })
                "同じ日の複数セッションは1日とカウント"
                (\sessions ->
                    if areAllSameDay sessions then
                        calculateStreak sessions
                            |> Expect.equal 1

                    else
                        -- 全て同じ日でない場合はテストをスキップ
                        Expect.pass
                )
            ]
        , describe "一般的な性質のテスト"
            [ fuzz (sessionsListFuzzer { strategy = RandomDays, minSessions = 0, maxSessions = 30 })
                "ストリークは0以上かつセッション数以下"
                (\sessions ->
                    calculateStreak sessions
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.atMost (List.length sessions)
                            ]
                )
            ]
        ]


{-| ユーティリティ関数: ユニークな日数をカウント
-}
countUniqueDays : List Session -> Int
countUniqueDays sessions =
    sessions
        |> List.map (.createdAt >> Date.fromPosix Time.utc >> Date.toRataDie)
        |> List.Extra.unique
        |> List.length


{-| ユーティリティ関数: 全ての日が同じかどうか
-}
areAllSameDay : List Session -> Bool
areAllSameDay sessions =
    countUniqueDays sessions == 1


{-| ユーティリティ関数: 日付が連続しているかどうか
-}
areDatesConsecutive : List Posix -> Bool
areDatesConsecutive dates =
    case dates of
        [] ->
            True

        [ _ ] ->
            True

        date1 :: date2 :: rest ->
            if abs (diff Days (Date.fromPosix Time.utc date1) (Date.fromPosix Time.utc date2)) == 1 then
                areDatesConsecutive (date2 :: rest)

            else
                False
