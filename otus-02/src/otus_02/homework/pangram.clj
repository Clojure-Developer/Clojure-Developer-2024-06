(ns otus-02.homework.pangram
  (:require [clojure.string :as string]
            [clojure.string :as str]))
(require '[clojure.string :as str])

(def letters
  [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z])

(def chars-set (set (map char (range (int \a) (+ (int \a) 26)))))

(defn is-pangram [test-string]
  (>= (count (reduce (fn [a e] (conj a e)) (set []) (str/lower-case test-string))) 26)
  
  )

(is-pangram "The quick brown fox jumps over the lazy dog")

