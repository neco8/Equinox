(ns equinox.pages.completion
  (:require [etaoin.api :as e]))

(def selectors
  {:next-button [{:tag "button" :aria-label "next"}]
   :finish-button [{:tag "button" :aria-label "finish"}]
   :finish-duration [{:tag "span" :aria-label "finish-duration"}]})

(defn proceed-complete-session
  [driver]
  (e/click driver (:next-button selectors)))

(defn finish-complete-session
  [driver]
  (e/click driver (:finish-button selectors)))

(defn get-finish-duration
  [driver]
  (e/get-element-value driver (:finish-duration selectors)))
