(ns equinox.specs.common
  (:require
   [clojure.test.check.generators :as gen]))

(def gen-char
  (gen/one-of [(gen/choose 0x20 0x7E)
               (gen/choose 0x3040 0x309F)  ;; ひらがな
               (gen/choose 0x30A0 0x30FF)  ;; カタカナ
               (gen/choose 0x4E00 0x9FFF)  ;; 漢字
               ]))
