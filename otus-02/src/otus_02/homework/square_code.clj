(ns otus-02.homework.square-code
  (:require [otus-02.homework.lib :refer [normalize]]
            [clojure.math :as math]
            [clojure.string :as string]))

(defn- get-dimensions
  [^Integer length]
  (let [sq (math/sqrt length)]
    [(-> sq
         math/ceil
         int) (int sq)]))


(defn encode-string
  [^String input]
  (let [normalized-input (normalize input)
        input-size (count normalized-input)
        [columns rows] (get-dimensions input-size)]
    (->> (concat normalized-input
                 (repeat (- (* columns rows) input-size) \space))
         (partition columns)
         (apply map str)
         (string/join \space))))

(defn decode-string
  [^String input]
  (let [input-size (count input)
        [_ rows] (get-dimensions input-size)]
    (->> (str input \space)
         (partition (inc rows))
         (map butlast)
         (apply map str)
         string/join
         string/trimr)))

(comment
  (let
    [source
       "If man was meant to stay on the ground, god would have given us roots."]
    (prn (encode-string source))
    (prn (decode-string (encode-string source)))))
