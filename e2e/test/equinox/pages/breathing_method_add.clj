(ns equinox.pages.breathing-method-add
  (:require [etaoin.api :as e]
            [clojure.test :refer [is]]))

(def selectors
  {:inhale-input [{:tag "input" :aria-label "inhale-duration-input"}]
   :inhale-hold-input [{:tag "input" :aria-label "inhale-hold-duration-input"}]
   :exhale-input [{:tag "input" :aria-label "exhale-duration-input"}]
   :exhale-hold-input [{:tag "input" :aria-label "exhale-hold-duration-input"}]

   :name-input [{:tag "input" :aria-label "breathing-method-name-input"}]
   :category-combobox [{:role "combobox" :aria-label "category-combobox"}]
   :category-option #(do [{:role "option" :data-id (str "option-" %)}])
   :category-input [{:tag "input" :role "combo-input"}]
   :category-create-option [{:role "create-new-option"}]

   :submit-breathing-method-button [{:tag "button" :aria-label "submit-breathing-method"}]})

(defn set-custom-breathing-parameters
  [driver {:keys [inhale inhale-hold exhale exhale-hold]}]
  (e/clear driver (:inhale-input selectors))
  (e/fill driver (:inhale-input selectors) (str inhale))
  (e/clear driver (:inhale-hold-input selectors))
  (e/fill driver (:inhale-hold-input selectors) (str inhale-hold))
  (e/clear driver (:exhale-input selectors))
  (e/fill driver (:exhale-input selectors) (str exhale))
  (e/clear driver (:exhale-hold-input selectors))
  (e/fill driver (:exhale-hold-input selectors) (str exhale-hold)))

(defn set-breathing-name [driver name]
  (e/clear driver (:name-input selectors))
  (e/fill driver (:name-input selectors) name))

(defn select-category [driver category-id]
  (:screenshot category-id)
  (e/click driver (:category-combobox selectors))
  (:screenshot category-id)
  (e/wait-visible driver ((:category-option selectors) category-id))
  (:screenshot category-id)
  (e/click driver ((:category-option selectors) category-id)))

(defn create-category [driver category-title]
  (e/click driver (:category-combobox selectors))
  (e/fill driver (:category-input selectors) category-title)
  (e/click driver (:category-create-option selectors)))

(defn submit-breathing-method [driver]
  (is (= (e/visible? driver (:submit-breathing-method-button selectors)) true))
  (e/click driver (:submit-breathing-method-button selectors)))
