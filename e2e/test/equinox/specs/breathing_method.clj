(ns equinox.specs.breathing-method
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [equinox.specs.category :as category]
            [equinox.specs.common :as common]))

(def gen-id
  (gen/fmap (fn [_] (random-uuid)) gen/nat))

;; 基本的な制約値の定義
(def min-name-length 1)
(def max-name-length 20)
(def max-stored-methods 1000)

(def gen-name
  (gen/fmap (fn [s] (apply str (map char s)))
            (gen/vector
             common/gen-char
             min-name-length
             max-name-length)))

;; 時間の定数（秒単位）
(def seconds 1)
(def minutes (* 60 seconds))
(def hours (* 60 minutes))
(def days (* 24 hours))

;; 時間の制約
(def min-phase-duration (* 1 seconds))       ; 最短1秒
(def min-hold-phase-duration 0)              ; ホールドフェーズは0秒可
(def max-phase-duration (* 10 minutes))         ; 1つのフェーズの最長時間

;; 基本的な値の検証
(s/def ::id uuid?)
(s/def ::name (s/and string?
                     (complement str/blank?)
                     #(<= min-name-length (count %) max-name-length)
                     #(re-matches #"[^\x00-\x1F\x7F]*" %)))  ; 制御文字を含まない

(s/def ::created-at inst?)

(defn gen-inst
  [{:keys [min-datetime-str max-datetime-str]
    :or {min-datetime-str
         "1970-01-01T00:00:00Z"
         max-datetime-str
         "2100-12-31T23:59:59Z"}}]
  (let [min-epoch (.toEpochMilli (java.time.Instant/parse min-datetime-str))
        max-epoch (.toEpochMilli (java.time.Instant/parse max-datetime-str))]
    (gen/let [epoch (gen/choose min-epoch max-epoch)]
      (gen/return (java.util.Date. epoch)))))

(s/def ::phase-type #{:inhale :inhale-hold :exhale :exhale-hold})

;; フェーズの時間検証（ホールドフェーズかどうかで最小値が変わる）
(defn validate-phase-duration [phase-type duration]
  (and (int? duration)
       (<= duration max-phase-duration)
       (if (#{:inhale-hold :exhale-hold} phase-type)
         (>= duration min-hold-phase-duration)
         (>= duration min-phase-duration))))

(defn gen-phase-duration [phase-type]
  (let [min-duration (if (#{:inhale-hold :exhale-hold} phase-type)
                       min-hold-phase-duration
                       min-phase-duration)]
    (gen/let [duration (gen/choose min-duration max-phase-duration)]
      (gen/return duration))))

(s/def ::inhale (s/and int? #(validate-phase-duration :inhale %)))
(s/def ::inhale-hold (s/and int? #(validate-phase-duration :inhale-hold %)))
(s/def ::exhale (s/and int? #(validate-phase-duration :exhale %)))
(s/def ::exhale-hold (s/and int? #(validate-phase-duration :exhale-hold %)))

;; 呼吸法全体の定義
(s/def ::category-id ::category/id)

(s/def ::breathing-method
  (s/keys :req-un [::id
                   ::name
                   ::category-id
                   ::created-at
                   ::inhale
                   ::inhale-hold
                   ::exhale
                   ::exhale-hold]))

(defn gen-breathing-method [categories]
  (gen/let [id gen-id
            name gen-name
            category-id (gen/elements (map :id categories))
            created-at (gen-inst
                              {:max-datetime-str (.toString (java.time.Instant/now))})
            inhale (gen-phase-duration :inhale)
            inhale-hold (gen-phase-duration :inhale-hold)
            exhale (gen-phase-duration :exhale)
            exhale-hold (gen-phase-duration :exhale-hold)]
    (gen/return {:id id
                 :name name
                 :category-id category-id
                 :created-at created-at
                 :inhale inhale
                 :inhale-hold inhale-hold
                 :exhale exhale
                 :exhale-hold exhale-hold})))

;; バリデーション用のヘルパー関数
(defn validate-breathing-method [method]
  (s/valid? ::breathing-method method))

;; コレクション全体のバリデーション
(defn validate-breathing-methods-collection [methods]
  (and (<= (count methods) max-stored-methods)
       (every? validate-breathing-method methods)))