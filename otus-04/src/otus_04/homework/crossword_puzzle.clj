(ns otus-04.homework.crossword-puzzle
  (:require
   [clojure.string :as string]))

"Возвращает решённый кроссворд. Аргумент является строкой вида

  +-++++++++
  +-++++++++
  +-++++++++
  +-----++++
  +-+++-++++
  +-+++-++++
  +++++-++++
  ++------++
  +++++-++++
  +++++-++++
  LONDON;DELHI;ICELAND;ANKARA

  Все строки вплоть до предпоследней описывают лист бумаги, а символами
  '-' отмечены клетки для вписывания букв. В последней строке перечислены
  слова, которые нужно 'вписать' в 'клетки'. Слова могут быть вписаны
  сверху-вниз или слева-направо."

;; Оригинал:
;; https://www.hackerrank.com/challenges/crossword-puzzle/problem

(def empty-s
  (str
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"
   "----------\n"))

(def s
  (str
   "+-++++++++\n"
   "+-++++++++\n"
   "+-++++++++\n"
   "+-----++++\n"
   "+-+++-++++\n"
   "+-+++-++++\n"
   "+++++-++++\n"
   "++------++\n"
   "+++++-++++\n"
   "+++++-++++\n"))

(def london-s
  (str
   "+L++++++++\n"
   "+O++++++++\n"
   "+N++++++++\n"
   "+D----++++\n"
   "+O+++-++++\n"
   "+N+++-++++\n"
   "+++++-++++\n"
   "++------++\n"
   "+++++-++++\n"
   "+++++-++++\n"))

(def filled
  (str
   "+l++++++++\n"
   "+o++++++++\n"
   "+n++++++++\n"
   "+delhi++++\n"
   "+o+++c++++\n"
   "+n+++e++++\n"
   "+++++l++++\n"
   "++ankara++\n"
   "+++++n++++\n"
   "+++++d++++\n"))

(def input
  "+-++++++++
  +-++++++++
  +-++++++++
  +-----++++
  +-+++-++++
  +-+++-++++
  +++++-++++
  ++------++
  +++++-++++
  +++++-++++
  LONDON;DELHI;ICELAND;ANKARA")

(def input1

  "++++++-+++
++------++
++++++-+++
++++++-+++
+++------+
++++++-+-+
++++++-+-+
++++++++-+
++++++++-+
++++++++-+
ICELAND;MEXICO;PANAMA;ALMATY")

(defn parse-to-s [s]
  (let [lines (clojure.string/split s #"\n")
        box (vec (map clojure.string/trim (take 10 lines)))
        words  (clojure.string/split (clojure.string/trim (last lines))  #";")]
    [box, words]))

(defn change-char-at [s pos new-char]
  (if (< pos (count s))
    (let [before (subs s 0 pos)
          after (subs s (inc pos))]
      (str before new-char after))
    s))

(defn change-box [box row col new-char]
  (let [line (nth box row)
        new-line (change-char-at line col new-char)]
    (assoc-in box [row] new-line)))

(defn can-fit-s? [box row col c]
  (let [existing (nth (nth box row) col)]
    (or (= existing \-) (= existing c))))

(defn put-in-row-s [box row col word]
  (if (or (> col (- (count (nth box 0)) 1))  (> row (- (count box) 1)))   nil
      (let [len (count (nth box 0)) word-len (count word)]
        (loop [index col
               word-index 0
               can-fit (can-fit-s? box row col (nth word word-index))
               new-box box]
          (if (or (= index len) (= word-index word-len))
            (if (and can-fit (= word-index word-len))  new-box nil)
            (recur
             (+ 1 index)
             (+ 1 word-index)
             (and can-fit (can-fit-s? new-box row index (nth word word-index)))
             (change-box new-box row index (nth word word-index))))))))

(defn put-in-column-s [box row col word]
  (let [len (count box)
        word-len (count word)]
    (loop [row-index row
           word-index 0
           can-fit (can-fit-s? box row col (nth word word-index))
           new-box box]
      (if (or (= row-index len) (= word-index word-len))
        (if (and can-fit (= word-index word-len)) new-box nil)
        (recur
         (+ 1 row-index)
         (+ 1 word-index)
         (and can-fit (can-fit-s? new-box row-index col (nth word word-index)))
         (change-box new-box row-index col (nth word word-index)))))))

(defn is-filled-s [box]
  (reduce (fn [a e] (and a (not (some (fn [c] (= \- c)) e)))) true box))

(defn get-positions [box]
  (let [len (- (count box) 1)]
    (loop
     [row 0
      col 0
      positions []]
      (if (and (= row len) (= col len))
        positions
        (recur
         (if (= col len) (+ row 1) row)
         (if (= col len) 0 (+ col 1))
         (if (= \- (get-in box [row col])) (conj positions [row col]) positions))))))

(defn gen-s [box positions word]
  (let [columns (filter some? (map (fn [[row col]] (put-in-column-s box row col word)) positions))
        rows (filter some? (map (fn [[row col]] (put-in-row-s box row col word)) positions))]
    (concat columns rows)))

(defn solve
  [input]
  (let [[box, words] (parse-to-s input)
        words-len (count words)
        positions (get-positions box)
        variants (gen-s box positions (nth words 0))]
    (loop [word-index 1 v variants found ()]
      (if  (or (= word-index words-len) (not-empty found)) (clojure.string/join "\n" (first v))
           (recur
            (inc word-index)
            (mapcat identity (map (fn [b] (gen-s b positions (nth words word-index))) v))
            (filter is-filled-s v))))))

; (solve input1)
