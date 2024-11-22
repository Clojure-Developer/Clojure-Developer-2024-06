(ns otus-02.homework.common-child
  (:require [clojure.set :refer [intersection]]
            [clojure.string :as string]))

(defn common-chars
  [f s]
  (let [first (set f) second (set s)] (intersection first second)))

(defn remove-char-at [s i] (str (subs s 0 i) (subs s (inc i))))

(defn all-children
  [s]
  (if (empty? s)
    #{}
    (reduce (fn [result idx]
              (let [new-str (remove-char-at s idx)]
                (into result (conj (all-children new-str) new-str))))
      #{}
      (range (count s)))))

;; Not optimized
;; will take long time to run on elements more then 8 or 9
;; And does not work on HARRY and SALLY =(((
(defn common-child-length-0
  [f s]
  (let [common-set (common-chars f s)
        filtered-f (-> common-set
                       (filter f)
                       string/join
                       all-children)
        filtered-s (-> common-set
                       (filter s)
                       string/join
                       all-children)]
    ((fnil #(apply max %) [0])
      (seq (map count (intersection filtered-f filtered-s))))))


;; Using recursion
(defn common-child-length
  [^String f ^String s]
  (let [inner-f (fn [inner-f-wr idx-f idx-s]
                  (let [inner-f-mem (partial inner-f-wr inner-f-wr)]
                    (cond (or (< idx-f 0) (< idx-s 0)) 0
                          (= (.charAt f idx-f) (.charAt s idx-s))
                            (inc (inner-f-mem (dec idx-f) (dec idx-s)))
                          :else (max (inner-f-mem (dec idx-f) idx-s)
                                     (inner-f-mem idx-f (dec idx-s))))))
        inner-f-mem (memoize inner-f)]
    (inner-f-mem inner-f-mem (dec (count f)) (dec (count s)))))


(comment
  (doseq [f [common-child-length common-child-length-0]]
    (time (doseq [test [(= (f "SHINCHAN" "NOHARAAA") 3)
                        ; (= (f "SHINCHANSHINCHANSHINCHAN"
                        ; "NOHARAAANOHARAAANOHARAAA") 9)
                        (= (f "HARRY" "SALLY") 2) (= (f "AA" "BB") 0)
                        (= (f "ABCDEF" "FBDAMN") 2)]]
            (prn test)))))
