(ns equinox.pages.breathing-method-add
  (:require [etaoin.api :as e]))

(def selectors
  {:inhale-input [{:tag "input" :aria-label "inhale-duration-input"}]
   :inhale-hold-input [{:tag "input" :aria-label "inhale-hold-duration-input"}]
   :exhale-input [{:tag "input" :aria-label "exhale-duration-input"}]
   :exhale-hold-input [{:tag "input" :aria-label "exhale-hold-duration-input"}]

   :name-input [{:tag "input" :aria-label "breathing-method-name-input"}]
   :category-combobox [{:role "combobox" :aria-label "category-combobox"}]

   :submit-breathing-method-button [{:tag "button" :aria-label "submit-breathing-method"}]})

(defn set-custom-breathing-parameters
  [driver {:keys [inhale inhale-hold exhale exhale-hold]}]
  (e/fill driver (:inhale-input selectors) (str inhale))
  (e/fill driver (:inhale-hold-input selectors) (str inhale-hold))
  (e/fill driver (:exhale-input selectors) (str exhale))
  (e/fill driver (:exhale-hold-input selectors) (str exhale-hold)))

(defn set-breathing-name [driver name]
  (e/fill driver (:name-input selectors) name))

;; ここはわからない。
(defn select-category [driver category]
  (e/select driver (:category-combobox selectors) category))

;; ここも実装が必要。comboboxの最低限の実装が終わってからわかる。
(defn create-category [driver catgory-title]
  (comment `(todo implement)))

(defn submit-breathing-method [driver]
  (e/click driver (:submit-breathing-method-button selectors)))
