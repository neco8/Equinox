(ns equinox.pages.source-selection
  (:require [etaoin.api :as e]))

(def selectors
  {;; オンラインソース選択
   :online-source-selection-button [{:tag "button" :aria-label "online-source-selection-button"}]
   :online-list [{:tag "ul" :aria-label "online-list"}]
   :online-item [{:tag "button" :aria-label "online-item"}]})

;; オンラインソース選択ルート

(defn click-online-source-selection-button
  [driver]
  (e/click driver (:online-source-selection-button selectors)))

(defn click-online-item
  [driver]
  (e/click driver (:online-item selectors)))