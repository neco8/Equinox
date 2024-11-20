module Test.CalcurateStatistics exposing (suite)

{-|


## 統計情報の整合性確認テスト


### テスト項目

  - 総セット数が練習日数以上であるべき
  - 練習日数がユニークな日付の数と一致するべき
  - 総秒数がセッションの合計時間と一致するべき
  - 直近の統計が総統計を超えないべき
  - 直近の統計が直近のセッションのみを含むべき
  - 直近統計の異なる日数範囲のテスト
      - n日範囲の直近統計が正しくフィルタされるべき
      - 0日範囲の直近統計は空の統計であるべき
      - 負の日数範囲の直近統計は空の統計であるべき


### TODO

  - [ ] TODO: タイムゾーンがちょっと怪しい。タイムゾーンを考慮したテストに修正する

-}

import Expect
import Fuzz exposing (Fuzzer)
import Fuzz.BreathingMethod exposing (breathingMethodFuzzer)
import Fuzz.Category exposing (categoryFuzzer)
import Fuzz.Common exposing (NonEmptyList, nonEmptyListFuzzer)
import Fuzz.Session exposing (sessionFuzzer)
import List.Extra
import Test exposing (..)
import Time
import Types.BreathingMethod exposing (BreathingMethod)
import Types.Category exposing (Category)
import Types.Session exposing (Session, fromDuration)
import Types.Statistics as Statistics exposing (recentDaysThreshold)


{-| 基準となる現在時刻（固定値）

時刻は `2024-03-04 00:00:00 UTC`

-}
referenceTime : Time.Posix
referenceTime =
    Time.millisToPosix 1709532000000



-- 日付の分布を制御するためのバリアント


{-| 日付の範囲を表す型

    type DateRange
        = WithinRecent
        | MoreThanWeekAgo

-}
type DateRange
    = WithinRecent -- 直近以内
    | MoreThanWeekAgo -- 7日以上前


{-| DateRange の Fuzzer
-}
dateRangeFuzzer : Fuzzer DateRange
dateRangeFuzzer =
    Fuzz.frequency
        [ ( 6, Fuzz.constant WithinRecent )
        , ( 4, Fuzz.constant MoreThanWeekAgo )
        ]


{-| 1日のミリ秒数を表す定数
-}
millisInDay : Int
millisInDay =
    24 * 60 * 60 * 1000


{-| createdAt タイムスタンプの Fuzzer
-}
createdAtFuzzer : Time.Posix -> Fuzzer Time.Posix
createdAtFuzzer baseTime =
    let
        baseMillis =
            Time.posixToMillis baseTime

        -- 指定範囲内の日時を生成
        createTimeInRange : Int -> Int -> Fuzzer Int
        createTimeInRange minDays maxDays =
            Fuzz.map2
                (\days mInDay ->
                    baseMillis
                        - (days * 24 * 60 * 60 * 1000)
                        - mInDay
                )
                (Fuzz.intRange minDays maxDays)
                (Fuzz.intRange 0 (millisInDay - 1))
    in
    dateRangeFuzzer
        |> Fuzz.andThen
            (\range ->
                case range of
                    WithinRecent ->
                        createTimeInRange 0 (recentDaysThreshold - 1)
                            |> Fuzz.map Time.millisToPosix

                    MoreThanWeekAgo ->
                        createTimeInRange recentDaysThreshold 30
                            |> Fuzz.map Time.millisToPosix
            )


{-| テストケース生成用の型
-}
type alias TestCase =
    { categories : NonEmptyList Category
    , sessions : List Session
    , breathingMethods : ( BreathingMethod, List BreathingMethod )
    }


{-| テストケースの Fuzzer
-}
testCaseFuzzer : Fuzzer TestCase
testCaseFuzzer =
    let
        categoriesFuzzer : Fuzzer (NonEmptyList Category)
        categoriesFuzzer =
            nonEmptyListFuzzer 1 5 categoryFuzzer

        breathingMethodsFuzzer : NonEmptyList Category -> Fuzzer (NonEmptyList BreathingMethod)
        breathingMethodsFuzzer categories =
            nonEmptyListFuzzer 1 5 (breathingMethodFuzzer categories referenceTime)

        sessionsFuzzer : NonEmptyList BreathingMethod -> BreathingMethod -> Fuzzer (List Session)
        sessionsFuzzer breathingMethods deletedBreathingMethod =
            Fuzz.listOfLengthBetween 20
                30
                (sessionFuzzer
                    breathingMethods
                    deletedBreathingMethod
                    (createdAtFuzzer referenceTime)
                )
    in
    categoriesFuzzer
        |> Fuzz.andThen
            (\categories ->
                breathingMethodsFuzzer categories
                    |> Fuzz.andThen
                        (\breathingMethods ->
                            breathingMethodFuzzer categories referenceTime
                                |> Fuzz.andThen
                                    (\deletedBreathingMethod ->
                                        sessionsFuzzer breathingMethods deletedBreathingMethod
                                            |> Fuzz.map
                                                (\sessions ->
                                                    TestCase
                                                        categories
                                                        sessions
                                                        breathingMethods
                                                )
                                    )
                        )
            )


{-| テストスイート
-}
suite : Test
suite =
    describe "統計情報の整合性確認"
        [ fuzz testCaseFuzzer "総セット数が練習日数以上であるべき" <|
            \testCase ->
                let
                    stats =
                        Statistics.calculateFromSessions testCase.sessions

                    recentStats =
                        Statistics.calculateRecentFromSessions recentDaysThreshold referenceTime testCase.sessions

                    message sts =
                        "総セット数: "
                            ++ String.fromInt sts.totalSets
                            ++ " が練習日数: "
                            ++ String.fromInt sts.totalPracticeDays
                            ++ " より少ない"
                in
                Expect.all
                    [ \_ ->
                        stats.totalSets
                            |> Expect.atLeast stats.totalPracticeDays
                            |> Expect.onFail (message stats)
                    , \_ ->
                        recentStats.totalSets
                            |> Expect.atLeast recentStats.totalPracticeDays
                            |> Expect.onFail (message recentStats)
                    ]
                    ()
        , fuzz testCaseFuzzer "練習日数がユニークな日付の数と一致するべき" <|
            \testCase ->
                let
                    -- [ ] TODO: タイムゾーンがちょっと怪しい。タイムゾーンを考慮したテストに修正する
                    stats =
                        Statistics.calculateFromSessions testCase.sessions

                    uniqueDatesCount =
                        testCase.sessions
                            |> List.map (.createdAt >> Time.posixToMillis >> dayFromMillis)
                            |> List.Extra.unique
                            |> List.length

                    message =
                        "練習日数: "
                            ++ String.fromInt stats.totalPracticeDays
                            ++ " がユニークな日付数: "
                            ++ String.fromInt uniqueDatesCount
                            ++ " と一致しない"
                in
                stats.totalPracticeDays
                    |> Expect.equal uniqueDatesCount
                    |> Expect.onFail message
        , fuzz testCaseFuzzer "総秒数がセッションの合計時間と一致するべき" <|
            \testCase ->
                let
                    stats =
                        Statistics.calculateFromSessions testCase.sessions

                    totalDuration =
                        List.sum (List.map (.duration >> fromDuration) testCase.sessions)

                    message =
                        "総秒数: "
                            ++ String.fromInt stats.totalSeconds
                            ++ " がセッションの合計時間: "
                            ++ String.fromInt totalDuration
                            ++ " と一致しない"
                in
                stats.totalSeconds
                    |> Expect.equal totalDuration
                    |> Expect.onFail message
        , fuzz testCaseFuzzer "直近の統計が総統計を超えないべき" <|
            \testCase ->
                let
                    stats =
                        Statistics.calculateFromSessions testCase.sessions

                    recentStats =
                        Statistics.calculateRecentFromSessions recentDaysThreshold referenceTime testCase.sessions

                    failureMessage stat recent total =
                        "直近の "
                            ++ stat
                            ++ ": "
                            ++ String.fromInt recent
                            ++ " が総 "
                            ++ stat
                            ++ ": "
                            ++ String.fromInt total
                            ++ " を超えている"
                in
                Expect.all
                    [ \_ ->
                        recentStats.totalSets
                            |> Expect.atMost stats.totalSets
                            |> Expect.onFail (failureMessage "セット" recentStats.totalSets stats.totalSets)
                    , \_ ->
                        recentStats.totalSeconds
                            |> Expect.atMost stats.totalSeconds
                            |> Expect.onFail (failureMessage "秒数" recentStats.totalSeconds stats.totalSeconds)
                    , \_ ->
                        recentStats.totalPracticeDays
                            |> Expect.atMost stats.totalPracticeDays
                            |> Expect.onFail (failureMessage "練習日数" recentStats.totalPracticeDays stats.totalPracticeDays)
                    ]
                    ()
        , fuzz testCaseFuzzer ("直近の統計が" ++ String.fromInt recentDaysThreshold ++ "日以内のセッションのみを含むべき") <|
            \testCase ->
                let
                    recentStats =
                        Statistics.calculateRecentFromSessions recentDaysThreshold referenceTime testCase.sessions

                    nDaysAgoMillis =
                        Time.posixToMillis referenceTime - (recentDaysThreshold * 24 * 60 * 60 * 1000)

                    recentSessions =
                        List.filter
                            (\session ->
                                Time.posixToMillis session.createdAt > nDaysAgoMillis
                            )
                            testCase.sessions

                    actualRecentTotalSeconds =
                        List.sum (List.map (.duration >> fromDuration) recentSessions)

                    message =
                        "直近の総秒数: "
                            ++ String.fromInt recentStats.totalSeconds
                            ++ " が直近のセッションの合計時間: "
                            ++ String.fromInt actualRecentTotalSeconds
                            ++ " と一致しない"
                in
                recentStats.totalSeconds
                    |> Expect.equal actualRecentTotalSeconds
                    |> Expect.onFail message
        , describe "直近統計の異なる日数範囲のテスト"
            [ fuzz2 testCaseFuzzer (Fuzz.intRange 1 30) "n日範囲の直近統計が正しくフィルタされるべき" <|
                \testCase nDays ->
                    let
                        recentStats =
                            Statistics.calculateRecentFromSessions nDays referenceTime testCase.sessions

                        nDaysAgoMillis =
                            Time.posixToMillis referenceTime - (nDays * 24 * 60 * 60 * 1000)

                        recentSessions =
                            List.filter
                                (\session ->
                                    Time.posixToMillis session.createdAt > nDaysAgoMillis
                                )
                                testCase.sessions

                        actualRecentTotalSeconds =
                            List.sum (List.map (.duration >> fromDuration) recentSessions)

                        message =
                            "直近 "
                                ++ String.fromInt nDays
                                ++ " 日間の総秒数: "
                                ++ String.fromInt recentStats.totalSeconds
                                ++ " が該当するセッションの合計時間: "
                                ++ String.fromInt actualRecentTotalSeconds
                                ++ " と一致しない"
                    in
                    recentStats.totalSeconds
                        |> Expect.equal actualRecentTotalSeconds
                        |> Expect.onFail message
            , fuzz testCaseFuzzer "0日範囲の直近統計は空の統計であるべき" <|
                \{ sessions } ->
                    let
                        emptyStats =
                            Statistics.calculateRecentFromSessions 0 referenceTime sessions
                    in
                    Expect.all
                        [ \_ -> emptyStats.totalSets |> Expect.equal 0
                        , \_ -> emptyStats.totalSeconds |> Expect.equal 0
                        , \_ -> emptyStats.totalPracticeDays |> Expect.equal 0
                        ]
                        ()
            , fuzz testCaseFuzzer "負の日数範囲の直近統計は空の統計であるべき" <|
                \{ sessions } ->
                    let
                        emptyStats =
                            Statistics.calculateRecentFromSessions -1 referenceTime sessions
                    in
                    Expect.all
                        [ \_ -> emptyStats.totalSets |> Expect.equal 0
                        , \_ -> emptyStats.totalSeconds |> Expect.equal 0
                        , \_ -> emptyStats.totalPracticeDays |> Expect.equal 0
                        ]
                        ()
            ]
        ]


{-| ミリ秒から日数を計算するヘルパー
-}
dayFromMillis : Int -> Int
dayFromMillis millis =
    millis // (24 * 60 * 60 * 1000)
