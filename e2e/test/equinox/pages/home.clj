(ns equinox.pages.home
  (:require [etaoin.api :as e]
            [equinox.pages.core :as core]))

(def selectors
  {:category-list #(do [{:tag "ul" :aria-label %}])
   :breathing-method-card #(do [{:tag "article" :aria-label %}])
   :start-button [{:tag "button" :aria-label "start-session-prepare"}]
   :add-new-button [{:tag "button" :aria-label "add-new-breathing-method"}]
   :settings-button [{:tag "button" :aria-label "settings"}]
   :statistics-tab [{:tag "button" :aria-label "statistics-tab"}]})

(defn open
  [driver]
  (e/go driver (core/->url (core/urls :home))))

(defn select-breathing-method
  [driver method-id]
  (e/wait-visible driver ((:breathing-method-card selectors) method-id))
  (e/click driver ((:breathing-method-card selectors) method-id)))

(defn click-start
  [driver]
  (e/click driver (:start-button selectors)))

(defn click-add-new
  [driver]
  (e/click driver (:add-new-button selectors)))

(defn click-settings
  [driver]
  (e/click driver (:settings-button selectors)))

(defn go-to-statistics
  [driver]
  (e/click driver (:statistics-tab selectors)))
