(ns equinox.mock.timer
  (:require [etaoin.api :as e]
            [clojure.spec.alpha :as s]))

(s/def ::driver any?)

(def mock-timer
  "// タイマーの状態を管理するグローバルオブジェクト
window.__timerMock = {
  currentTime: Date.now(),
  timers: [],
  
  // 時間を進める
  advanceTime(ms) {
    this.currentTime += ms;
    this.checkTimers();
  },

  // タイマーをチェックして期限が来たものを実行
  checkTimers() {
    const now = this.currentTime;
    const expiredTimers = this.timers.filter(t => t.nextTick <= now);
    expiredTimers.forEach(timer => {
      if (timer.interval) {
        timer.nextTick = now + timer.interval;
        timer.callback();
      } else {
        this.clearTimer(timer.id);
        timer.callback();
      }
    });
  },

  // タイマーをクリア
  clearTimer(id) {
    this.timers = this.timers.filter(t => t.id !== id);
  }
};

// オリジナルのタイマー関数を保存
const originalSetTimeout = window.setTimeout;
const originalSetInterval = window.setInterval;
const originalClearTimeout = window.clearTimeout;
const originalClearInterval = window.clearInterval;
const originalDate = window.Date;

// モックタイマーを有効化
window.enableMockTimer = () => {
  window.setTimeout = (callback, delay) => {
    const id = Math.random();
    window.__timerMock.timers.push({
      id,
      callback,
      nextTick: window.__timerMock.currentTime + delay,
      interval: null
    });
    return id;
  };

  window.setInterval = (callback, interval) => {
    const id = Math.random();
    window.__timerMock.timers.push({
      id,
      callback,
      nextTick: window.__timerMock.currentTime + interval,
      interval: interval // 修正: `interval;` から `interval: interval` に変更
    });
    return id;
  };

  window.clearTimeout = (id) => {
    window.__timerMock.clearTimer(id);
  };

  window.clearInterval = (id) => {
    window.__timerMock.clearTimer(id);
  };

  // Date.now()をモック
  window.Date.now = () => window.__timerMock.currentTime;

  // 現在時刻をリセット
  window.__timerMock.currentTime = Date.now();
};

// オリジナルのタイマーを復元
window.restoreTimer = function() {
  window.setTimeout = originalSetTimeout;
  window.setInterval = originalSetInterval;
  window.clearTimeout = originalClearTimeout;
  window.clearInterval = originalClearInterval;
  window.Date = originalDate;
};
")

(defn init-mock-timer
  "タイマーモックを初期化する
   driver: Etaoinのドライバーインスタンス"
  [driver]
  (e/js-execute driver mock-timer)
  ;; タイマーモックを有効化
  (e/js-execute driver "window.enableMockTimer()"))

;; init-mock-timer 関数のスペック
(s/fdef init-mock-timer
  :args (s/cat :driver ::driver)
  :ret nil?)

(defn cleanup-mock-timer
  "タイマーモックをクリアする
   driver: Etaoinのドライバーインスタンス"
  [driver]
  (e/js-execute driver "window.restoreTimer()"))

(s/fdef cleanup-mock-timer
  :args (s/cat :driver ::driver)
  :ret nil?)

;; fixtureの定義
(defn with-mock-timer
  "タイマーモックを有効化して関数を実行する
    fixtureにすると、etaoinのテスト実行自体瞬間的に行われてしまうので、advance-timeを利用するときのみ利用する
    driver: Etaoinのドライバーインスタンス
    f: 実行する関数
   "
  [driver f]
  (try
    (println "\n\ninit mock timer")
    (init-mock-timer driver)
    (Thread/sleep 100)
    (f)
    (finally
      (cleanup-mock-timer driver))))

(s/fdef with-mock-timer
  :args (s/cat :driver ::driver :f ifn?)
  :ret nil?)

(defn advance-timer!
  "タイマーを指定ミリ秒進める
   driver: Etaoinのドライバーインスタンス
   ms: 進める時間（ミリ秒）"
  [driver ms]
  (println "\n\nadvance timer" ms)
  (e/js-execute driver (str "window.__timerMock.advanceTime(" ms ")")))

(s/fdef advance-timer!
  :args (s/cat :driver ::driver :ms nat-int?)
  :ret nil?)

(defn get-current-time
  "現在のモック時刻を取得する（ミリ秒）
   driver: Etaoinのドライバーインスタンス"
  [driver]
  (e/js-execute driver "window.__timerMock.currentTime"))

(s/fdef get-current-time
  :args (s/cat :driver ::driver)
  :ret nat-int?)

(defn reset-timer!
  "タイマーの状態をリセットする
   driver: Etaoinのドライバーインスタンス"
  [driver]
  (e/js-execute driver "window.__timerMock.currentTime = Date.now(); window.__timerMock.timers = []"))

(s/fdef reset-timer!
  :args (s/cat :driver ::driver)
  :ret nil?)
