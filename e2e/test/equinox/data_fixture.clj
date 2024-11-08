(ns equinox.data-fixture
  (:require [clojure.walk :as walk]
            [cheshire.core :as json]
            [etaoin.api :as e]
            [clojure.spec.alpha :as s]
            [equinox.browser :refer [get-driver]]))

(s/def ::driver any?) ;; 実際にはetaoinのWebDriverオブジェクトを指定
(s/def ::key string?)
(s/def ::value any?)
(s/def ::prepared-data map?)
(s/def ::data any?) ;; JSONに変換可能な任意のデータ構造
(s/def ::objects (s/coll-of any? :kind vector?))
(s/def ::key-fn ifn?)
(s/def ::breathing-methods (s/coll-of any?))
(s/def ::categories (s/coll-of any?))
(s/def ::sessions (s/coll-of any?))
(s/def ::fixture (s/fspec :args (s/cat) :ret any?))

(defn- convert-inst-to-epoch
  "Converts an Instant to an epoch timestamp"
  [v]
  (if (inst? v)
    (.getTime v)
    v))

(s/fdef convert-inst-to-epoch
  :args (s/cat :v any?)
  :ret any?)

(defn- convert-uuid-to-string
  "Converts a UUID to a string"
  [v]
  (if (uuid? v)
    (str v)
    v))

(s/fdef convert-uuid-to-string
  :args (s/cat :v any?)
  :ret any?)

(defn- prepare-for-json
  "Prepare Clojure data structures for JSON serialization:
   - Converts keywords to strings
   - Converts UUIDs to strings
   - Converts inst? to epoch timestamps"
  [data]
  (walk/stringify-keys
   (walk/postwalk
    convert-uuid-to-string
    (walk/postwalk convert-inst-to-epoch data))))

(s/fdef prepare-for-json
  :args (s/cat :data any?)
  :ret any?)

(defn vector->map [key-fn objects]
  (reduce (fn [acc obj] (assoc acc (key-fn obj) obj)) {} objects))

(s/fdef vector->map
  :args (s/cat :key-fn ::key-fn :objects ::objects)
  :ret map?)

(defn- set-local-storage! [driver key value]
  (e/js-execute driver
                (str "window.localStorage.setItem(`"
                     key
                     "`, JSON.stringify("
                     (json/generate-string value)
                     "));")))

(s/fdef set-local-storage!
  :args (s/cat :driver ::driver :key ::key :value ::value)
  :ret any?)

(defn clear-local-storage!
  "Clear all test data from localStorage"
  [driver]
  (e/go driver "http://localhost:3000")
  (e/js-localstorage-clear driver))

(s/fdef clear-local-storage!
  :args (s/cat :driver ::driver)
  :ret any?)

(defn- prepare-data-for-fixture
  [& {:keys [breathing-methods categories sessions]}]
  (let [prepared-data
        (into {}
              (map (fn [[k v]]
                     (let [mapped (vector->map :id v)
                           prepared (prepare-for-json mapped)]
                       [(str "EQUINOX_" (name k)) prepared]))
                   {:breathing-methods breathing-methods
                    :categories categories
                    :sessions sessions}))]
    prepared-data))

(s/fdef prepare-data-for-fixture
  :args (s/keys* :opt-un [::breathing-methods ::categories ::sessions])
  :ret ::prepared-data)

(defn use-data-fixture [{:keys [breathing-methods categories sessions]}]
  (fn [f]
    (let [driver (get-driver)]
      (try
        (e/go driver "http://localhost:3000")
        (doseq [[k v] (prepare-data-for-fixture :breathing-methods breathing-methods
                                                :categories categories
                                                :sessions sessions)]
          (set-local-storage! driver k v))
        (e/reload driver)
        (f)
        (finally
          (clear-local-storage! driver))))))

(s/fdef use-data-fixture
  :args (s/cat :data ::data)
  :ret ::fixture)