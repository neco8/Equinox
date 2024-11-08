(ns equinox.specs.session
  (:require [clojure.spec.alpha :as s]
            [equinox.specs.breathing-method :as bm]
            [clojure.test.check.generators :as gen]
            [equinox.specs.category :as category]))

;; 時間の定数（秒単位）
(def seconds 1)
(def minutes (* 60 seconds))
(def hours (* 60 minutes))
(def days (* 24 hours))

;; 時間の制約
(def min-session-duration (* 1 minutes))     ; 最短1分
(def max-session-duration (* 10 hours))       ; 最長10日間

(s/def ::id uuid?)
(s/def ::inhale ::bm/inhale)
(s/def ::inhale-hold ::bm/inhale-hold)
(s/def ::exhale ::bm/exhale)
(s/def ::exhale-hold ::bm/exhale-hold)
(s/def ::breathing-method-id ::bm/id)
(s/def ::breathing-method-name ::bm/name)
;; durationは秒単位であることに留意
(s/def ::duration (s/and int?
                         #(<= min-session-duration %)
                         #(<= % max-session-duration)))
(s/def ::created-at inst?)

(s/def ::session
  (s/keys :req [::id
                ::inhale
                ::inhale-hold
                ::exhale
                ::exhale-hold
                ::breathing-method-id
                ::breathing-method-name
                ::duration
                ::created-at]))

(def gen-id
  gen/uuid)
(def gen-duration
  (gen/choose min-session-duration max-session-duration))

(defn gen-created-at []
  ;; 今日 過去7日間 7日前 7日より前
  (let [today (java.time.Instant/now)
        seven-days-ago (java.time.Instant/ofEpochMilli
                        (- (.toEpochMilli today) (* 7 days 1000)))]
    (gen/one-of (mapv
                 #(case %
                    :today (gen/return (java.util.Date. (.toEpochMilli today)))
                    :recent-7-days (bm/gen-inst {:min-datetime-str (.toString seven-days-ago)
                                                 :max-datetime-str (.toString today)})
                    :7-days-ago (gen/return (java.util.Date. (.toEpochMilli seven-days-ago)))
                    :before-7-days (bm/gen-inst {:max-datetime-str (.toString seven-days-ago)}))
                 [:today :recent-7-days :7-days-ago :before-7-days]))))

(defn gen-session
  [breathing-methods]
  (gen/let [categories (gen/vector category/gen-category 1 3)
            id gen-id
            duration gen-duration
            created-at (gen-created-at)

            chosen-breathing-method
            (gen/frequency [[5 (bm/gen-breathing-method categories)]
                            [95 (gen/elements breathing-methods)]])]
    (gen/return
     {:id id

      :inhale (:inhale chosen-breathing-method)
      :inhale-hold (:inhale-hold chosen-breathing-method)
      :exhale (:exhale chosen-breathing-method)
      :exhale-hold (:exhale-hold chosen-breathing-method)
      :breathing-method-id (:id chosen-breathing-method)
      :breathing-method-name (:name chosen-breathing-method)

      :duration duration
      :created-at created-at})))
