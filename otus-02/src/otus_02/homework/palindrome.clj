(ns otus-02.homework.palindrome
  (:require [clojure.string :as string]
            [clojure.string :as str]))

(defn remove-non-alpha [s]
  (apply str (re-seq #"[a-zA-Z]+" s)))


  (defn aux [s len pos]
    (cond
      (>= pos len) true
      (= (nth s pos) (nth s (- len pos))) (aux s len (+ 1 pos))
      :else false))

(defn is-palindrome [test-string]
  (let [
        s (-> test-string remove-non-alpha str/lower-case)
        len (- (count s) 1)
        ]
   (aux s len  0) 
    
    )
  )

(-> "Was it a cat I saw?" remove-non-alpha str/lower-case )


(is-palindrome "Was it a cat I saw?")
(print 21)
