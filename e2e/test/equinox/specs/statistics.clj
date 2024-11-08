(ns equinox.specs.statistics
  (:require [clojure.spec.alpha :as s]
            [equinox.specs.session :as session]))

(s/def ::total-sets int?)
(s/def ::total-minutes int?)
(s/def ::total-practice-days int?)

(s/def ::statistics
  (s/keys :req-un [::total-sets
                   ::total-minutes
                   ::total-practice-days]))


;; fdef
(s/fdef gen-statistics
  :args (s/cat :sessions (s/coll-of ::session/session))
  :ret ::statistics)

(s/fdef latest-date-inst
  :args (s/cat :dates (s/coll-of inst?))
  :ret inst?)

(defn date-to-local-date
  "Converts java.util.Date to java.time.LocalDate"
  [date]
  (let [instant (.toInstant date)
        zone-id (java.time.ZoneId/systemDefault)
        zoned-date-time (.atZone instant zone-id)]
    (.toLocalDate zoned-date-time)))

(defn truncate-to-date
  "Converts inst to start of day"
  [inst]
  (let [local-date (date-to-local-date inst)
        start-of-day (.atStartOfDay local-date (java.time.ZoneId/systemDefault))
        start-of-day-instant (.toInstant start-of-day)]
    (java.util.Date/from start-of-day-instant)))

(defn latest-date-inst
  "Takes a sequence of inst dates and returns the latest date, truncated to start of day"
  [dates]
  (let [truncated-dates (map truncate-to-date dates)
        latest-date (last (sort truncated-dates))]
    latest-date))

(defn ->statistics
  [sessions]
  (let [total-sets (count sessions)
        ;; sessionに保存されているdurationは秒単位
        total-seconds (reduce + (map :duration sessions))
        ;; 分単位に変換 (切り捨て)
        total-minutes (int (/ total-seconds 60))
        ;; ローカルの日付に変換して重複を除いて数える
        total-practice-days
        (count
         (distinct
          (map #(-> % :created-at date-to-local-date)
               sessions)))
        ;; セッションの中で、最も新しい日付を取得
        last-calculated-at
        (latest-date-inst (map :created-at sessions))]
    {:total-sets total-sets
     :total-minutes total-minutes
     :total-practice-days total-practice-days
     :last-calculated-at last-calculated-at}))
