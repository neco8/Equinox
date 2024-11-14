(ns equinox.screenshot
  (:require [etaoin.api :as e]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest]]
            [clojure.walk :as walk])
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDateTime)))

;; SCREENSHOT

(defn- generate-timestamp []
  (.format (LocalDateTime/now)
           (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss")))

(s/fdef generate-timestamp
  :args (s/cat)
  :ret string?)

(defn ensure-screenshot-dir [base-dir timestamp]
  (let [dir (io/file base-dir timestamp)]
    (.mkdirs dir)
    dir))

(s/fdef ensure-screenshot-dir
  :args (s/cat :base-dir string? :timestamp string?)
  :ret #(instance? java.io.File %))

(defn- transform-screenshot-forms [test-name timestamp form]
  (let [counter (atom 0)]
    (walk/postwalk
     (fn [node]
       (if (and (sequential? node)
                (= :screenshot (first node)))
         (let [filename (second node)
               current-count (format "%03d" @counter)]
           (swap! counter inc)
           `(let [screenshot-dir# (ensure-screenshot-dir "screenshots" ~timestamp)
                  screenshot-path# (str (.getPath screenshot-dir#) "/" ~test-name "_" ~current-count "_" ~filename ".png")
                  screenshot-html-path# (str (.getPath screenshot-dir#) "/" ~test-name "_" ~current-count "_" ~filename ".html")
                  html-content# (e/get-element-inner-html ~'driver {:tag :html})]
              (e/screenshot ~'driver screenshot-path#)
              (spit screenshot-html-path# html-content#)))
         node))
     form)))

(s/fdef transform-screenshot-forms
  :args (s/cat :test-name string? :timestamp string? :form any?)
  :ret any?)

(def ^:private screenshot-timestamp (generate-timestamp))

(defmacro defscreenshottest [name & body]
  (let [test-name (str name)
        transformed-body (transform-screenshot-forms
                          test-name
                          screenshot-timestamp
                          `(do ~@body))]
    `(deftest ~name
       ~transformed-body)))

(s/fdef defscreenshottest
  :args (s/cat :name symbol? :body (s/* any?)))
