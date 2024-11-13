(ns equinox.mock.api
  (:require [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.response :refer [response]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]))

;; オンライン呼吸法のデフォルトデータ
(def default-online-methods
  [{:id "online-1"
    :name "Box Breathing"
    :inhale 4
    :inhale-hold 4
    :exhale 4
    :exhale-hold 4}
   {:id "online-2"
    :name "4-7-8 Breathing"
    :inhale 4
    :inhale-hold 7
    :exhale 8
    :exhale-hold 0}
   {:id "online-3"
    :name "Resonant Breathing"
    :inhale 5
    :inhale-hold 0
    :exhale 5
    :exhale-hold 0}])

;; アプリケーション状態を保持するatom
(def app-state (atom {:online-methods default-online-methods}))

(defroutes app-routes
    ;; オンライン呼吸法一覧を取得
  (GET "/api/v1/breathing-methods" []
    (response {:breathing-methods (:online-methods @app-state)}))

    ;; オンライン呼吸法の重複チェック
  (POST "/api/v1/breathing-methods/check-duplicate" {body :body}
    (let [{:keys [inhale inhale-hold exhale exhale-hold]} body
          exists? (some #(and (= (:inhale %) inhale)
                              (= (:inhale-hold %) inhale-hold)
                              (= (:exhale %) exhale)
                              (= (:exhale-hold %) exhale-hold))
                        (:online-methods @app-state))]
      (response {:duplicate exists?})))

  (route/not-found (response {:error "Not Found"})))

;; 環境ごとのCORS設定
(def cors-config
  {:development
   {:origins [#"http://localhost:3000"]
    :methods [:get :post :options]}

   :test  ;; TODO: テスト環境の設定を更新
   {:origins [#"http://localhost:\.*"]
    :methods [:get :post :options]}

   :production  ;; TODO: 本番環境の設定を更新
   {:origins [#"https://.*\.your-domain.com"]
    :methods [:get :post]}})

;; アプリケーションを生成する関数
(defn create-app
  ([env]
   (let [{:keys [origins methods]} (get cors-config env)]
     (-> app-routes
         wrap-json-response
         (wrap-cors :access-control-allow-origin origins
                    :access-control-allow-methods methods)))))

;; TODO: 環境変数を受け取るようにする。dev, test, prodを取得。そして、アプリケーションのCORS設定をできるように。でも、今はdevelopmentのみでいいかも。

;; サーバー起動用の関数
(defn start-mock-server
  ([port]
   (start-mock-server port {}))
  ([port {:keys [online-methods]}]
   (when online-methods
     (swap! app-state assoc :online-methods online-methods))
   (run-jetty (create-app :development) {:port port :join? false})))

;; E2Eテスト用の関数
(defn with-api
  "オンラインAPIのモックサーバーを立ち上げてE2Eテストを実行するマクロ"
  ([fn]
   (with-api {} fn))
  ([options fn]
   (let [port 3001
         server (start-mock-server port options)]
     (try
       (fn)
       (finally
         (.stop server))))))


(defn change-online-methods
  "オンライン呼吸法のデータを変更する"
  [online-methods]
  (swap! app-state assoc :online-methods online-methods))