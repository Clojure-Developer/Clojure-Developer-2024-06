(ns otus-02.homework.palindrome
  (:require [clojure.string :as string]
            [clojure.test :refer [is]]))

(def ^:private alpha (set "abcdefghijklmnopqrstuvwxyz"))

(defn is-palindrome
  [test-string]
  (->> test-string
       string/lower-case
       (filter alpha)
       (#(= % (reverse %)))))

(defn is-palindrome-0
  "Using vector instead of seq."
  [test-string]
  (->> test-string
       string/lower-case
       (filterv alpha)
       (#(= % (reverse %)))))

(defn is-palindrome-1
  "Using rseq instead of reverse."
  [test-string]
  (->> test-string
       string/lower-case
       (filterv alpha)
       (#(= % (rseq %)))))

(defn is-palindrome-2
  "More efficient. Using filter based on ascii code. 3.5X to speed from the first implementation."
  [test-string]
  (->> test-string
       string/lower-case
       (filterv #(< 96 (int %) 123))
       (#(= % (rseq %)))))

(comment
  (doseq [f [is-palindrome is-palindrome-0 is-palindrome-1 is-palindrome-2]]
    (time (dotimes [_ 100]
            (doseq [s ["civic" "tattarrattat" "taco cat" "no lemon, no melon"
                       "Eva, can I see bees in a cave?" "Was it a cat I saw?"]]
              (is (f s)))
            (doseq [s ["civics" "They all have one thing" "knock on the door"]]
              (is (not (f s))))))))
