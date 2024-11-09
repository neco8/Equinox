(ns equinox.specs.category
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [equinox.specs.common :as common]))


(def gen-id
  (gen/fmap (fn [_] (random-uuid)) gen/nat))
(s/def ::id uuid?)

;; 基本的な制約値の定義
(def min-title-length 1)
(def max-title-length 20)
(s/def ::title (s/and string?
                      #(<= min-title-length (count %) max-title-length)))

(defn truncate-left [s max-length]
  (let [length (count s)]
    (if (<= length max-length)
      s
      (subs s (- length max-length)))))

(def gen-title
  (gen/fmap (fn [s] (truncate-left (str "category-" (apply str (map char s))) max-title-length))
            (gen/vector
             common/gen-char
             min-title-length
             max-title-length)))

(s/def ::category
  (s/keys :req [::id
                ::title]))

(def gen-category
  (gen/let [id gen-id
            title gen-title]
    (gen/return {:id id
                 :title title})))