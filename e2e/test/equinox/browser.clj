(ns equinox.browser
  (:require [etaoin.api :as e]
            [clojure.spec.alpha :as s]))

(def ^:private driver-atom (atom nil))

(defn get-driver []
  @driver-atom)

(s/fdef get-driver
  :args (s/cat)
  :ret (s/nilable any?)) ;; 'driver'オブジェクトまたはnilを返す

;; [ ] TODO: CI上ではheadlessモードでブラウザを起動する

(defn browser-fixture [f]
  (let [driver (e/chrome)]
    (try
      (println "\n\nbrowser-fixture: opening browser")
      (reset! driver-atom driver)
      (e/go driver "http://localhost:3000")
      (f)
      (finally
        (reset! driver-atom nil)
        (e/quit driver)))))

(s/fdef browser-fixture
  :args (s/cat :f ifn?)
  :ret any?)
