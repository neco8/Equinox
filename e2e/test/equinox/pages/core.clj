(ns equinox.pages.core
  (:require [etaoin.api :as e]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def urls
  {:home "/"
   :preset-preparation "/breathing-methods/session/preparation/:id"
   :manual-preparation "/breathing-methods/session/preparation"
   :preset-session "/breathing-methods/session/running/:id"
   :manual-session "/breathing-methods/session/running"
   :preset-session-completion "/breathing-methods/session/completion/:id"
   :manual-session-completion "/breathing-methods/session/completion"
   :statistics "/statistics"
   :settings "/settings"
   :source-selection "/breathing-methods/source-selection"
   :edit "/breathing-methods/edit/:id"})

(defn ->url [path]
  (str "http://localhost:3000" path))

(defn path->pattern
  "Converts a path with :params into a regex pattern.
   Example: '/breathing-methods/session/prepare/:id' -> #'/breathing-methods/session/prepare/.+'"
  [path]
  (if (string? path)
    (if (.contains path ":")
      (re-pattern
       (-> path
           (str/replace #":[^/]+" "[^/]+")
           (str/replace #"/" "\\/")))
      path)
    path))

(s/def ::url-key
  (set (keys urls)))

(s/fdef current-url?
  :args (s/cat :driver any? :expected-key ::url-key)
  :ret any?)

(defn current-url? [driver expected-key]
  (let [current-path (-> (e/get-url driver)
                         java.net.URL.
                         .getPath)
        url (get urls expected-key)
        expected (path->pattern url)]
    (if (instance? java.util.regex.Pattern expected)
      (re-matches expected current-path)
      (= current-path expected))))
