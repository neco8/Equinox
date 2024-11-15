(ns equinox.test.scenarios
  (:require
   [clojure.test :refer [is testing use-fixtures]]
   [clojure.test.check.generators :as gen]
   [clojure.spec.test.alpha :as stest]

   [equinox.screenshot :refer [defscreenshottest]]
   [equinox.data-fixture :refer [use-data-fixture]]

   [etaoin.api :as e]

   [equinox.pages.core :as core]
   [equinox.pages.home :as home]
   [equinox.pages.preparation :as preparation]
   [equinox.pages.session :as session]
   [equinox.pages.completion :as completion]
   [equinox.pages.statistics :as statistics]
   [equinox.pages.source-selection :as source-selection]
   [equinox.pages.breathing-method-add :as breathing-method-add]

   [equinox.specs.breathing-method :as sbm]
   [equinox.specs.session :as sse]
   [equinox.specs.category :as sca]

   [equinox.browser :refer [get-driver browser-fixture]]

   [equinox.mock.timer :refer [with-mock-timer advance-timer!] :as timer]
   [equinox.mock.api :refer [with-api change-online-methods]]))

(stest/instrument)

(use-fixtures :each browser-fixture)

(def existing-breathing-method-flow-test-data
  (let [categories (gen/generate
                    (gen/vector sca/gen-category 1 10))
        breathing-methods (gen/generate
                           (gen/vector (sbm/gen-breathing-method categories) 1 10))
        sessions (gen/generate
                  (gen/vector (sse/gen-session breathing-methods) 1 10))

        selected-breathing-method (gen/generate
                                   (gen/elements breathing-methods))
        duration (gen/generate
                  (gen/let [duration (gen/choose sse/min-session-duration sse/max-session-duration)]
                    (gen/return (* 60 (quot duration 60)))))]
    {:categories categories
     :breathing-methods breathing-methods
     :sessions sessions
     :selected-breathing-method selected-breathing-method
     :duration duration}))

(defscreenshottest 既存に存在する呼吸法を選択する
  ((use-data-fixture existing-breathing-method-flow-test-data)
   (fn []
     (let [driver (get-driver)
           {:keys [categories breathing-methods selected-breathing-method duration]}
           existing-breathing-method-flow-test-data]

       (testing "✨ ホームページが正しく表示されるかURLを確認"
         (home/open driver)
         (e/wait-visible driver {:role "home"} {:timeout 10})
         (is (core/current-url? driver :home) "ホームページが開けません"))

       (testing "🔍 ホームページの必要な要素が全部そろっているか確認"
         (doseq [q
                 (flatten
                  [(map (home/selectors :category-list) (map (comp str :id) categories))
                   (map (home/selectors :breathing-method-card) (map (comp str :id) breathing-methods))
                   (map home/selectors [:start-button :add-new-button :settings-button :statistics-tab])])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "ホーム"))

       (testing "👆 呼吸法を選ぶ"
         (home/select-breathing-method driver (:id selected-breathing-method))
         (e/wait-visible driver {:role "preparation"})
         (is (core/current-url? driver :preset-preparation) "準備ページへの移動に失敗しました")
         (doseq [q (map preparation/selectors [:duration-input :start-button :inhale :inhale-hold :exhale :exhale-hold :backdrop])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "セッション準備"))

       (testing "⏱️ セッションの準備をする"
         (preparation/set-duration driver duration)
         (is (= (preparation/get-duration-value driver) (str (quot duration 60))) "時間の設定が反映されていません"))

       (with-mock-timer driver
         #(do
            (testing "⏲️ セッションをスタート！"
              (preparation/start-session driver)
              (e/wait-visible driver {:role "session"})
              (is (core/current-url? driver :preset-session) "セッション中ページへの移動に失敗しました")
              (doseq [q (map session/selectors [:timer :instruction :pause-button])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
               ;; TODO: セッション中の停止・再開の画面要素が存在することをテスト
              (:screenshot "セッション中"))

            (testing "🎯 セッションを最後まで実行"
              (advance-timer! driver (* (- duration 1) 1000))
               ;; まだ終了していないことを確認
              (is (not (core/current-url? driver :preset-session-completion)) "テスト想定より早くセッションが終わってしまいました")
              (advance-timer! driver 1000)
               ;; 終了したことを確認
              (session/wait-for-completion driver)
              (is (core/current-url? driver :preset-session-completion) "セッション完了ページへの移動に失敗しました")
              (doseq [q (map completion/selectors [:next-button :finish-duration])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
               ;; TODO: セッション完了の完了ボタンの表示テスト
              (:screenshot "セッション完了"))))

       (testing "📊 統計を確認してみましょう"
         (completion/proceed-complete-session driver)
         (let [finish-duration (completion/get-finish-duration driver)]
           (is (= finish-duration duration)
               (str "実施時間が想定と異なります。想定: " duration "秒, 実際: " finish-duration "秒")))
         (completion/finish-complete-session driver)
         (e/wait-visible driver {:role "statistics"})
         (is (core/current-url? driver :statistics) "統計ページへの移動に失敗しました")
         (doseq [q (map statistics/selectors [:settings-button :streak-display :recent-sets :recent-minutes :total-sets :total-minutes :total-practice-days :home-tab :start-button])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "統計"))

       (testing "🏠 ホームに戻る"
         (statistics/go-home driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページへの移動に失敗しました")
         (:screenshot "ホーム"))))))

(def custom-breathing-method-flow-test-data
  (let [categories (gen/generate
                    (gen/vector sca/gen-category 1 10))
        breathing-methods (gen/generate
                           (gen/vector (sbm/gen-breathing-method categories) 1 10))
        sessions (gen/generate (gen/vector (sse/gen-session breathing-methods) 1 10))
        custom-breathing-method (gen/generate (sbm/gen-breathing-method categories))

        duration (gen/generate
                  (gen/let [duration
                            (gen/choose sse/min-session-duration sse/max-session-duration)]
                    (gen/return (* 60 (quot duration 60)))))]
    {:categories categories
     :breathing-methods breathing-methods
     :custom-breathing-method custom-breathing-method
     :sessions sessions
     :duration duration}))

(defscreenshottest 自分で呼吸法をカスタマイズしてセッションを行う
  ((use-data-fixture custom-breathing-method-flow-test-data)
   (fn []
     (let [driver (get-driver)
           {:keys [custom-breathing-method duration]} custom-breathing-method-flow-test-data]

       (testing "✨ ホームページが正しく表示されるかURLを確認"
         (home/open driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページが開けません")
         (:screenshot "ホーム"))

       (testing "▶️ ホームからセッションをスタート"
         (home/click-start driver)
         (e/wait-visible driver {:role "preparation"})
         (is (core/current-url? driver :manual-preparation) "カスタム準備ページへの移動に失敗しました")
         (doseq [q (map preparation/selectors [:duration-input :start-button :inhale-input :inhale-hold-input :exhale-input :exhale-hold-input])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "カスタムセッション準備"))

       (testing "⏱️ セッションの準備をする！"
         (let
          [{:keys [inhale inhale-hold exhale exhale-hold]}
           custom-breathing-method]
           (preparation/set-custom-breathing-parameters
            driver
            {:inhale inhale
             :inhale-hold inhale-hold
             :exhale exhale
             :exhale-hold exhale-hold}))

         (preparation/set-duration driver duration)

         (let [{:keys [inhale inhale-hold exhale exhale-hold]} custom-breathing-method]
           (is (= (preparation/get-inhale-value driver) (str inhale)) "吸う時間の設定が反映されていません")
           (is (= (preparation/get-inhale-hold-value driver) (str inhale-hold)) "吸ったあと止める時間の設定が反映されていません")
           (is (= (preparation/get-exhale-value driver) (str exhale)) "吐く時間の設定が反映されていません")
           (is (= (preparation/get-exhale-hold-value driver) (str exhale-hold)) "吐いたあと止める時間の設定が反映されていません")
           (is (= (preparation/get-duration-value driver) (str (quot duration 60))) "セッション時間の設定が反映されていません"))
         (:screenshot "カスタムセッション準備入力"))

       (with-mock-timer driver
         #(do
            (testing "⏲️ タイマーをスタート！"
              (preparation/start-session driver)
              (e/wait-visible driver {:role "session"})
              (is (core/current-url? driver :manual-session) "セッション中ページへの移動に失敗しました")
              (doseq [q (map session/selectors [:timer :instruction :pause-button])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
              (:screenshot "カスタムセッション中"))

            (testing "🎯 セッションを完了まで実行"
              (advance-timer! driver (* (- duration 1) 1000))
              (is (not (core/current-url? driver :manual-session-completion)) "テスト想定より早くセッションが終わってしまいました")
              (advance-timer! driver 1000)

              (session/wait-for-completion driver)
              (is (core/current-url? driver :manual-session-completion) "セッション完了ページへの移動に失敗しました")
              (doseq [q (map completion/selectors [:next-button :finish-duration])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
              (:screenshot "カスタムセッション完了"))))

       (testing "📊 統計を確認してみましょう"
         (completion/proceed-complete-session driver)
         (let [finish-duration (completion/get-finish-duration driver)]
           (is (= finish-duration duration)
               (str "実施時間が想定と異なります。想定: " duration "秒, 実際: " finish-duration "秒")))
         (completion/finish-complete-session driver)
         (e/wait-visible driver {:role "statistics"})
         (is (core/current-url? driver :statistics) "統計ページへの移動に失敗しました")
         (:screenshot "統計"))

       (testing "🏠 ホームに戻る"
         (statistics/go-home driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページへの移動に失敗しました")
         (:screenshot "ホーム"))))))


;; TODO: セッション中断フローの際、finish-durationが適切になっているかを確認する

(def online-source-flow-test-data
  (let [categories (gen/generate
                    (gen/vector sca/gen-category 1 10))
        breathing-methods (gen/generate
                           (gen/vector (sbm/gen-breathing-method categories) 1 10))
        sessions (gen/generate
                  (gen/vector (sse/gen-session breathing-methods) 1 10))

        ;; online breeathing methodsには category-id、created-atがつかないのでdissocする
        online-breathing-methods (gen/generate
                                  (gen/vector
                                   (gen/let [bm (sbm/gen-breathing-method categories)]
                                     (gen/return (dissoc bm :category-id :created-at)))))
        selected-online-breathing-method (gen/generate
                                          (gen/elements online-breathing-methods))

        edit-breathing-method (gen/generate
                               (gen/let [type (gen/elements #{:edit :not-edit})
                                         breathing-method (case type
                                                            :edit (sbm/gen-breathing-method categories)
                                                            :not-edit (gen/return nil))]
                                 (gen/return {:type type :breathing-method breathing-method})))
        edit-category (gen/generate
                       (gen/let [type (gen/elements #{:add :existing})
                                 category (case type
                                            :add sca/gen-category
                                            :existing (gen/elements categories))]
                         (gen/return {:type type :category category})))]
    {:categories categories
     :breathing-methods breathing-methods
     :sessions sessions
     :online-breathing-methods online-breathing-methods
     :selected-online-breathing-method selected-online-breathing-method
     :edit-breathing-method edit-breathing-method
     :edit-category edit-category}))

(defscreenshottest オンラインソースから呼吸法を追加する
  ((use-data-fixture online-source-flow-test-data)
   (fn []
     (let [driver (get-driver)
           {:keys [online-breathing-methods selected-online-breathing-method edit-breathing-method edit-category breathing-methods categories]} online-source-flow-test-data]

       (testing "✨ ホームページが正しく表示されるかURLを確認"
         (home/open driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページが開けません")
         (:screenshot "ホーム"))

       (with-api
         #(do
            (change-online-methods online-breathing-methods)

            (testing "➕ 新規呼吸法追加ボタンをクリック"
              (home/click-add-new driver)
               ;; ソース選択画面への遷移を確認
              (e/wait-visible driver {:role "source-selection"})
              (is (core/current-url? driver :source-selection) "ソース選択画面への移動に失敗しました")
              (doseq [q (map source-selection/selectors [:online-source-selection-button :manual-source-selection-button])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
              (:screenshot "ソース選択"))

            (testing "👆️ オンラインソースを選択"
              (source-selection/click-online-source-selection-button driver)
              (e/wait-visible driver {:role "online-list"})
              (doseq [q (flatten
                         [(map source-selection/selectors [:online-list])
                                  ;; オンラインソースがすべて存在することを確認する
                          (map (fn [bm]
                                 ((source-selection/selectors :online-item)
                                  {:name (:name bm)
                                   :id (:id bm)}))
                               online-breathing-methods)])]
                (e/wait-visible driver q {:timeout 10})
                (is (e/visible? driver q) (str "見つからない要素があります → " q)))
              (:screenshot "オンラインソースリスト")
              (source-selection/click-online-item driver (:name selected-online-breathing-method) (:id selected-online-breathing-method))
              (e/wait-visible driver {:role "edit"}))

               ;; 新規呼吸法追加画面へ遷移する
            (is (core/current-url? driver :add) "呼吸法新規追加画面への遷移に失敗しました")
            (:screenshot "呼吸法新規追加")))

       (letfn [(add-breathing-method
                 []
                 (case (:type edit-category)
                   :add (breathing-method-add/create-category driver
                                                              (:title (:category edit-category)))
                   :existing (breathing-method-add/select-category driver
                                                                   (:id (:category edit-category))))
                 (:screenshot "カテゴリー選択")

                 (breathing-method-add/submit-breathing-method driver)
                 (e/wait-visible driver {:role "home"} {:timeout 10})
                 (is (core/current-url? driver :home) "ホームページへの遷移に失敗しました"))]
          ;; TODO: 前の画面から受け取った値が最初から表示されているか確認する
         (case (:type edit-breathing-method)
           :edit (testing "➕️ 呼吸法を編集した後、追加する"
                   (breathing-method-add/set-custom-breathing-parameters driver (:breathing-method edit-breathing-method))
                   (breathing-method-add/set-breathing-name driver (:name (:breathing-method edit-breathing-method)))
                   (:screenshot "呼吸法編集")
                   (add-breathing-method))

           :not-edit (testing "➡️ 呼吸法を編集せず、カテゴリーのみ追加して追加する"
                       (:screenshot "呼吸法未編集")
                       (add-breathing-method))))

       (testing "🌟 ホームページで呼吸法が正しく追加されていることを確認"
         ;; カテゴリー、呼吸法を追加できているかどうかを確認。
         (is (= (count (e/query-all driver {:aria-label "category"}))
                (case (:type edit-category)
                  :add (inc (count categories))
                  :existing (count categories))) (str "カテゴリーが追加されていません → " (:title (:category edit-category))))
         (is (= (count (e/query-all driver {:aria-label "breathing-method-card"}))
                (inc (count breathing-methods)))
             (str "呼吸法が追加されていません → " (:name (case (:type edit-breathing-method)
                                              :edit (:breathing-method edit-breathing-method)
                                              :not-edit selected-online-breathing-method)))))))))

(def manual-source-flow-test-data
  (let [categories (gen/generate
                    (gen/vector sca/gen-category 1 10))
        breathing-methods (gen/generate
                           (gen/vector (sbm/gen-breathing-method categories) 1 10))
        sessions (gen/generate
                  (gen/vector (sse/gen-session breathing-methods) 1 10))

        edit-breathing-method (gen/generate
                               (sbm/gen-breathing-method categories))
        edit-category (gen/generate
                       (gen/let [type (gen/elements #{:add :existing})
                                 category (case type
                                            :add sca/gen-category
                                            :existing (gen/elements categories))]
                         (gen/return {:type type :category category})))]
    {:categories categories
     :breathing-methods breathing-methods
     :sessions sessions
     :edit-breathing-method edit-breathing-method
     :edit-category edit-category}))

(defscreenshottest 手動で呼吸法を追加する
  ((use-data-fixture manual-source-flow-test-data)
   (fn []
     (let [driver (get-driver)
           {:keys [edit-breathing-method edit-category breathing-methods categories]} manual-source-flow-test-data]

       (testing "✨ ホームページが正しく表示されるかURLを確認"
         (home/open driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページが開けません")
         (:screenshot "ホーム"))

       (testing "➕ 新規呼吸法追加ボタンをクリック"
         (home/click-add-new driver)
               ;; ソース選択画面への遷移を確認
         (e/wait-visible driver {:role "source-selection"})
         (is (core/current-url? driver :source-selection) "ソース選択画面への移動に失敗しました")
         (:screenshot "ソース選択"))

       (testing "✍️ 手動ソースを選択"
         (source-selection/click-manual-source-selection-button driver)
         (e/wait-visible driver {:role "edit"})
         ;; 新規呼吸法追加画面へ遷移する
         (is (core/current-url? driver :add) "呼吸法新規追加画面への遷移に失敗しました")
         (:screenshot "呼吸法新規追加"))

       (testing "➕️ 呼吸法を編集し、追加する"
         (breathing-method-add/set-custom-breathing-parameters driver edit-breathing-method)
         (breathing-method-add/set-breathing-name driver (:name edit-breathing-method))
         (case (:type edit-category)
           :add (breathing-method-add/create-category driver
                                                      (:title (:category edit-category)))
           :existing (breathing-method-add/select-category driver
                                                           (:id (:category edit-category))))
         (:screenshot "呼吸法編集")
         (breathing-method-add/submit-breathing-method driver)
         (e/wait-visible driver {:role "home"} {:timeout 10})
         (is (core/current-url? driver :home) "ホームページへの遷移に失敗しました")
         (:screenshot "ホーム"))

       (testing "🌟 ホームページで呼吸法が正しく追加されていることを確認"
         ;; カテゴリー、呼吸法を追加できているかどうかを確認。
         (is (= (count (e/query-all driver {:aria-label "category"}))
                (case (:type edit-category)
                  :add (inc (count categories))
                  :existing (count categories)))
             (str "カテゴリーが追加されていません → " (:title (:category edit-category))))
         (is (= (count (e/query-all driver {:aria-label "breathing-method-card"}))
                (inc (count breathing-methods))) (str "呼吸法が追加されていません → " (:name edit-breathing-method))))))))
