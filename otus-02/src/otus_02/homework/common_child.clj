(ns otus-02.homework.common-child)

;; Строка называется потомком другой строки,
;; если она может быть образована путем удаления 0 или более символов из другой строки.
;; Буквы нельзя переставлять.
;; Имея две строки одинаковой длины, какую самую длинную строку можно построить так,
;; чтобы она была потомком обеих строк?

;; Например 'ABCD' и 'ABDC'

;; Эти строки имеют два дочерних элемента с максимальной длиной 3, ABC и ABD.
;; Их можно образовать, исключив D или C из обеих строк.
;; Ответ в данном случае - 3

;; Еще пример HARRY и SALLY. Ответ будет - 2, так как общий элемент у них AY

; (defn lol [n]
;   (loop [i 0 r nil]
;     (if (> i n)
;       r
;       (loop [j 0 s nil]
;         (if (< j n) (recur (+ 1 j) (conj s [i j])) s)))))
;
; (lol 10)
;
; (defn look [s1 s2 i j t]
;   (let
;    [s1-elem (nth s1 (- i 1))
;     s2-elem (nth s2 (- j 1))
;     i-1j-1 (get-in t [(- i 1) (- j 1)])
;     i-1j (get-in t [(- i 1) j])
;     ij-1 (get-in t [i (- j 1)])]
;     (if (= s1-elem s2-elem) (assoc-in t [i j] (+ i-1j-1 1)) (assoc-in t [i j] (max i-1j ij-1)))))
;
; (get-in [[0,0,0,0,0,0], [0],[0],[0],[0],[0],[0]]  [1 0])
; (assoc-in [[0,0,0,0,0,0], [0],[0],[0],[0],[0],[0]] [1 1] 12)
;
; (assoc-in [[0,0,0,0,0,0], [0],[0],[0],[0],[0],[0]] [2 1] 1)
;
; (range 1 (+ 2 (count "HARRY")))
;
; (def i1 (look "HARRY" "SALLY" 1 1 [[0,0,0,0,0,0], [0], [0], [0], [0], [0], [0], [0]]))
; (def i2 (look "HARRY" "SALLY" 1 2 i1))
; (def i3 (look "HARRY" "SALLY" 1 3 i2))
; (def i4 (look "HARRY" "SALLY" 1 4 i3))
; (def i5 (look "HARRY" "SALLY" 1 5 i4))
;
; (def i6 (look "HARRY" "SALLY" 2 1 i5))
; (def i7 (look "HARRY" "SALLY" 2 2 i6))
; (def i8 (look "HARRY" "SALLY" 2 3 i7))
; (def i9 (look "HARRY" "SALLY" 2 4 i8))
; (def i10 (look "HARRY" "SALLY" 2 5 i9))
;
; (def q (vec (repeat (+ 1 (count "HARRY")) 0)))
; (def w (vec (repeat (+ 1 (count "SALLY")) [0])))
; (def e (vec (concat q w)))


(defn common-child-length [s1 s2]
  (let [look (fn [s1 s2 i j t]
               (let
                [s1-elem (nth s1 (- i 1))
                 s2-elem (nth s2 (- j 1))
                 i-1j-1 (get-in t [(- i 1) (- j 1)])
                 i-1j (get-in t [(- i 1) j])
                 ij-1 (get-in t [i (- j 1)])]
                 (if (= s1-elem s2-elem) (assoc-in t [i j] (+ i-1j-1 1)) (assoc-in t [i j] (max i-1j ij-1)))))
        t [(vec (repeat (+ 1 (count s1))  0))]
        r (vec (repeat (+ 1 (count s2)) [0]))
        table (vec (concat t r))
        irange (range 1 (+ 1 (count s1)))
        jrange (range 1 (+ 1 (count s2)))]
    (get-in (reduce (fn [t i] (reduce (fn [t1 j] (look s1 s2 i j t1)) t  jrange)) table irange) [(count s1) (count s1)])))

(common-child-length "SHINCHAN" "NOHARAAA")


