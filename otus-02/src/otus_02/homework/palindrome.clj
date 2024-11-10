(ns otus-02.homework.palindrome
  (:require [clojure.string :as string]))

(defn is-palindrome
  "Func for check strings on palyndrome"
  [s]
  {:pre [(not= s nil)]}
  (let [test-string (as-> s $
                        (string/lower-case $)
                        (string/replace $ #"[^a-z]" ""))]
    (= test-string (str/reverse test-string))))
