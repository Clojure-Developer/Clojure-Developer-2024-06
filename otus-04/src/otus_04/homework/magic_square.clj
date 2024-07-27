(ns otus-04.homework.magic-square)

;; Оригинальная задача:
;; https://www.codewars.com/kata/570b69d96731d4cf9c001597
;;
;; Подсказка: используйте "Siamese method"
;; https://en.wikipedia.org/wiki/Siamese_method

(defn move [n point]
  (let [[row col] point
        try-row (- row 1)
        try-col (+ 1 col)
        next-row (if (< try-row 0) (- n 1) try-row)
        next-col (if (>= try-col n) 0 try-col)]
    [next-row next-col]))

(defn down [n point]
  (let [[row col] point
        try-row (+ row 1)
        next-row (if (>= try-row n) 0 try-row)]
    [next-row col]))

(defn magic-square [n]
  (let [initial-row 0
        initial-col (int (Math/floor (/ n 2)))]

    (loop [m 1
           point [initial-row initial-col]
           prev-point point
           box (vec (repeat n (vec (repeat n 0))))
           count 0]
      (cond
        (= count (* n n)) box
        (zero? (get-in box point)) (recur (+ 1 m) (move n point) point (assoc-in box point m) (+ 1 count))
        :else (recur m (down n prev-point) point box count)))))



