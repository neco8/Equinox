(ns equinox.pages.source-selection
  (:require [etaoin.api :as e]))

(def selectors
  {;; オンラインソース選択
   :online-source-selection-button [{:tag "button" :aria-label "online-source-selection-button"}]
   :online-list [{:tag "ul" :aria-label "online-list"}]
   :online-item (fn [{:keys [name id]}]
                  [{:tag "button" :aria-label name :data-id id}])})

;; オンラインソース選択ルート

(defn click-online-source-selection-button
  [driver]
  (e/click driver (:online-source-selection-button selectors)))

(defn click-online-item
  [driver breathing-name id]
  (e/click driver ((:online-item selectors) {:name breathing-name :id id})))