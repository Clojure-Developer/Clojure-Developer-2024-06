(ns otus-02.homework.lib
  (:require [clojure.string :as string]))

(defn- is-alpha?
  [c]
  (let [i (int c)]
    (cond (and (> i 96) (< i 123)) true
          (and (> i 64) (< i 91)) true
          :else false)))

(defn normalize
  [s]
  (->> s
       string/lower-case
       vec
       (filterv is-alpha?)
       (apply str)))
