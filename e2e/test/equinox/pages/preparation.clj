(ns equinox.pages.preparation
  (:require [etaoin.api :as e]))

(def selectors
  {:duration-input [{:tag "input" :aria-label "session-duration-input"}]
   :start-button [{:tag "button" :aria-label "start-session"}]

   :inhale [{:tag "span" :aria-label "inhale"}]
   :inhale-hold [{:tag "span" :aria-label "inhale-hold"}]
   :exhale [{:tag "span" :aria-label "exhale"}]
   :exhale-hold [{:tag "span" :aria-label "exhale-hold"}]
   :inhale-input [{:tag "input" :aria-label "inhale-duration-input"}]
   :inhale-hold-input [{:tag "input" :aria-label "inhale-hold-duration-input"}]
   :exhale-input [{:tag "input" :aria-label "exhale-duration-input"}]
   :exhale-hold-input [{:tag "input" :aria-label "exhale-hold-duration-input"}]
   :backdrop [{:tag "div" :aria-label "backdrop"}]})

(defn set-duration [driver duration]
  (e/fill driver (:duration-input selectors) (str duration)))

(defn set-custom-breathing-parameters [driver {:keys [inhale inhale-hold exhale exhale-hold]}]
  (e/fill driver (:inhale-input selectors) (str inhale))
  (e/fill driver (:inhale-hold-input selectors) (str inhale-hold))
  (e/fill driver (:exhale-input selectors) (str exhale))
  (e/fill driver (:exhale-hold-input selectors) (str exhale-hold)))

(defn start-session [driver]
  (e/click driver (:start-button selectors)))

(defn get-duration-value [driver]
  (e/get-element-value driver (:duration-input selectors)))

(defn get-inhale-value [driver]
  (e/get-element-value driver (:inhale-input selectors)))

(defn get-inhale-hold-value [driver]
  (e/get-element-value driver (:inhale-hold-input selectors)))

(defn get-exhale-value [driver]
  (e/get-element-value driver (:exhale-input selectors)))

(defn get-exhale-hold-value [driver]
  (e/get-element-value driver (:exhale-hold-input selectors)))

(defn back-home [driver]
  (e/click driver (:backdrop selectors)))

;; TODO: backdropのテストを実装する