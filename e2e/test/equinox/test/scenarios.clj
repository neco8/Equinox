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

   [equinox.mock.timer :refer [with-mock-timer advance-timer!] :as timer]))

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
                  (gen/choose sse/min-session-duration sse/max-session-duration))]
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
         (is (= (preparation/get-duration-value driver) (str duration)) "時間の設定が反映されていません"))

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
                  (gen/choose sse/min-session-duration sse/max-session-duration))]
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
         (e/click driver (home/selectors :start-button))
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
           (is (= (preparation/get-duration-value driver) (str duration)) "セッション時間の設定が反映されていません"))
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
         ;; TODO: ここで、finish-durationが想定のdurationと同じであることを確認する。
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
                                     (dissoc bm :category-id :created-at))))
        selected-online-breathing-method (gen/generate
                                          (gen/elements online-breathing-methods))

        ;; 編集するなら存在し、しないならnilとなるような値
        custom-edit-breathing-method (gen/generate
                                      (gen/one-of [(sbm/gen-breathing-method categories)
                                                   (gen/return nil)]))
        custom-edit-create-category (when custom-edit-breathing-method
                                      (gen/generate
                                       (gen/one-of [sca/gen-category
                                                    (gen/return nil)])))]
    {:categories categories
     :breathing-methods breathing-methods
     :sessions sessions
     :online-breathing-methods online-breathing-methods
     :selected-online-breathing-method selected-online-breathing-method
     :custom-edit-breathing-method custom-edit-breathing-method
     :custom-edit-create-category custom-edit-create-category}))

(defscreenshottest オンラインソースから呼吸法を追加する
  ((use-data-fixture online-source-flow-test-data)
   (fn []
     (let [driver (get-driver)
           {:keys [online-breathing-methods selected-online-breathing-method custom-edit-breathing-method custom-edit-create-category]} online-source-flow-test-data]

       (testing "✨ ホームページが正しく表示されるかURLを確認"
         (home/open driver)
         (e/wait-visible driver {:role "home"})
         (is (core/current-url? driver :home) "ホームページが開けません")
         (:screenshot "ホーム"))

       (testing "➕ 新規呼吸法追加ボタンをクリック"
         (e/click driver (home/selectors :add-new-button))
         ;; ソース選択画面への遷移を確認
         (e/wait-visible driver {:role "source-selection"})
         (is (core/current-url? driver :source-selection) "ソース選択画面への移動に失敗しました")
         (doseq [q (map source-selection/selectors [:online-source-selection-button])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "ソース選択"))

       (testing "👆️ オンラインソースを選択"
         (source-selection/click-online-source-selection-button driver)
         (doseq [q (flatten [(map source-selection/selectors [:online-list])
                            ;; オンラインソースがすべて存在することを確認する
                             (map (fn [bm] ((source-selection/selectors :online-item) {:name (:name bm) :id (:id bm)})) online-breathing-methods)])]
           (e/wait-visible driver q {:timeout 10})
           (is (e/visible? driver q) (str "見つからない要素があります → " q)))
         (:screenshot "オンラインソースリスト")
         (source-selection/click-online-item driver (:name selected-online-breathing-method) (:id selected-online-breathing-method))

         ;; 新規呼吸法追加画面へ遷移する
         (is (core/current-url? driver :add) "呼吸法新規追加画面への遷移に失敗しました")
         (:screenshot "呼吸法新規追加"))

       (letfn [(add-breathing-method
                 []
                 (breathing-method-add/submit-breathing-method driver)

                 (e/wait-visible driver {:role "home"})
                 (is (core/current-url? driver :home) "ホームページへの遷移に失敗しました"))]
         (if custom-edit-breathing-method
           (testing "呼吸法を編集した後、追加する"
             (breathing-method-add/set-custom-breathing-parameters driver custom-edit-breathing-method)
             (breathing-method-add/set-breathing-name driver (:name custom-edit-breathing-method))
             (breathing-method-add/select-category driver (:category-id custom-edit-breathing-method))
             (when custom-edit-create-category
               (breathing-method-add/create-category driver (:title custom-edit-create-category)))
             (:screenshot "呼吸法編集")
             (add-breathing-method))

           (testing "呼吸法を編集せず、追加する"
             (:screenshot "呼吸法未編集")
             (add-breathing-method))))

       (testing "ホームページで呼吸法が正しく追加されていることを確認"
         ;; この部分のテスト、呼吸法は名前しか知らずIDは自動生成となるため、Homeのselectorsを拡充する必要がある。現在は生のqueryを記述する。
         (when custom-edit-create-category
           (e/visible? driver {:tag "ul" :aria-label (:title custom-edit-create-category)}))
         (e/visible? driver {:tag "article" :aria-label (:name custom-edit-breathing-method)}))))))
