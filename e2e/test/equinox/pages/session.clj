(ns equinox.pages.session
  (:require [etaoin.api :as e]))

(def selectors
  {:timer [{:role "timer" :aria-label "session-timer"}]
   :instruction [{:tag "article" :aria-label "session-instruction"}]
   :pause-button [{:tag "button" :aria-label "pause"}]
   :resume-button [{:tag "button" :aria-label "resume"}]
   :stop-button [{:tag "button" :aria-label "stop"}]})

(defn wait-for-completion [driver]
  (e/wait-visible driver {:role "session-completion"}))

