(ns equinox.pages.statistics
  (:require [etaoin.api :as e]))

 ;;ここに存在してるのは設定ボタンとストリーク表示と直近7日間のセット分数合計のセット分数後合計日清とホームボタンとすぐにスタートするやつ
(def selectors
  {:settings-button [{:tag "button" :aria-label "settings"}]
   :streak-display [{:tag "div" :aria-label "streak-display"}]
   :recent-sets [{:tag "span" :aria-label "recent-sets"}]
   :recent-minutes [{:tag "span" :aria-label "recent-minutes"}]
   :total-sets [{:tag "span" :aria-label "total-sets"}]
   :total-minutes [{:tag "span" :aria-label "total-minutes"}]
   :total-practice-days [{:tag "span" :aria-label "total-practice-days"}]
   :home-tab [{:tag "button" :aria-label "home-tab"}]
   :start-button [{:tag "button" :aria-label "start-session-prepare"}]})

(defn click-settings
  [driver]
  (e/click driver (:settings-button selectors)))

(defn click-start
  [driver]
  (e/click driver (:start-button selectors)))

(defn go-home
  [driver]
  (e/click driver (:home-tab selectors)))

;; TODO: statisticsのイベント、設定画面へ。あと、