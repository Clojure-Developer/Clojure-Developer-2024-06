(ns otus-02.homework.pangram
  (:require [clojure.string :as string]))

(defn is-pangram [test-string]
  "Func for check strings on pangram"
  {:pre [(not= test-string nil)]}
  (let [set-chars (-> test-string
                       string/lower-case
                       (string/replace #"[^a-z]" "")
                       set)]
    (= (count set-chars) 26)))
