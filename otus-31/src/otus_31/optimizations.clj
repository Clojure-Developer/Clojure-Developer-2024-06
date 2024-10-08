(ns otus-31.optimizations
  (:require [clojure.string :as str]
            [criterium.core :refer [quick-bench]]))

(comment
  (set! *warn-on-reflection* false)
  (set! *unchecked-math* false))

(defn rand-string [length]
  (str/join (repeatedly length #(rand-nth "abcdefghijklmnopqrstuvwxyz"))))

;; * basis
(def ^:const charset "abcdefghijklmnopqrstuvwxyz")
(def ^:const charset-size (count charset))

(defn- -ch-index [ch]
  (- (int ch) (int \a)))

(defn vfreq+ [acc-v s]
  (reduce (fn [acc x] (update acc (-ch-index x) inc))
          acc-v s))

(defn vfreq- [acc-v s]
  (reduce (fn [acc x] (update acc (-ch-index x) dec))
          acc-v s))

(defn scramble? [source sub]
  (let [source-freq (vfreq+ (->> (repeat 0) (take charset-size) vec)
                            source)]
    (->> (vfreq- source-freq sub)
         (not-any? neg?))))

(comment
  (quick-bench (scramble? "abcdefghijklmnopqrstuvwxyz" "bookkeeper"))
  ;; 8.5us
)

;; * transients
(defn vrange [n]
  (loop [i 0 v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))

(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

(comment
  (do
    (println :vrange)
    (quick-bench (vrange 100000))
    (println :vrange2)
    (quick-bench (vrange2 100000))))

(defn transient-update! [t idx f]
  (let [val (get t idx)]
    (assoc! t idx (f val))))

(defn scramble?-transient [source-string sub-string]
  (let [source-freq (reduce (fn [acc x]
                              (transient-update! acc (-ch-index x) inc))
                            (transient (vec (take charset-size (repeat 0)))) source-string)]
    (->> sub-string
         (reduce (fn [acc x]
                   (transient-update! acc (-ch-index x) dec))
                 source-freq)
         persistent!
         (not-any? neg?))))

(comment
  (quick-bench (scramble?-transient "abcdefghijklmnopqrstuvwxyz" "bookkeeper"))
  ;; 5us
  )

;; * java arrays
(defn- aswap!
  [a idx f]
  (->> (aget a idx)
       (f)
       (aset-int a idx)))

(defn scramble?-arrays
  [source-string sub-string]
  (let [char-counts (long-array charset-size)]
    (doseq [ch source-string]
      (aswap! char-counts (-ch-index ch) inc))
    ;; not-any? works as loop here, iterates over sub-string
    (not-any? #(neg? (aswap! char-counts (-ch-index %) dec)) sub-string)))

(comment
  (quick-bench (scramble?-arrays "abcdefghijklmnopqrstuvwxyz" "bookkeeper"))
  )

;; * reflection
(comment
  (set! *warn-on-reflection* true))

(comment
  (def s "123")
  (def i 123)
  (.toString s)
  (.toString ^String s)
  (scramble?-arrays "abcdefghijklmnopqrstuvwxyz" "bookkeeper"))

(defn- aswap!
  [^longs a idx f]
  (->> (aget a idx)
       (f)
       (aset-int a idx)))

(comment
  (quick-bench (scramble?-arrays "abcdefghijklmnopqrstuvwxyz" "bookkeeper")))

;; * (un)boxing
(comment
  (set! *unchecked-math* :warn-on-boxed))

(defn- aswap!
  ^long [^longs a idx f]
  (->> (aget a idx)
       (f)
       (aset-int a idx)))

(comment
  (quick-bench (scramble?-arrays "abcdefghijklmnopqrstuvwxyz" "bookkeeper")))


;; * all of them
(comment
  (def s1 (rand-string 10000))
  (def s2 (rand-string 10000))
  (quick-bench (scramble? s1 s2))
  (quick-bench (scramble?-transient s1 s2))
  (quick-bench (scramble?-arrays s1 s2)))

;; * task
;; Как можно оптимизировать данный код?
(require 'clj-java-decompiler.core)
(defn distance ^double [^double x1 ^double y1
                        ^double x2 ^double y2]

  (Math/sqrt
   (+
    (Math/pow (- x2 x1) 2)
    (Math/pow (- y2 y1) 2))))

(quick-bench
 (->> (range 1000)
      (filter odd?)
      (map #(* % %))
      (map inc)
      (reduce +)))

(quick-bench
 (->> (range 1000)
      (filterv odd?)
      (mapv #(* % %))
      (mapv inc)
      (reduce +)))

(quick-bench
    (transduce
     (comp (filter odd?)
           (map #(* % %))
           (map inc))
     +
     (range 1000)))
