(ns otus-04.homework.scramblies)

;; Оригинальная задача:
;; https://www.codewars.com/kata/55c04b4cc56a697bb0000048

(defn aux [s]
  (loop [l (count s)
         i 0
         dict {}]

    (if (= i l)
      dict
      (recur l (+ 1 i) (assoc dict (nth s i) (+ 1 (get dict (nth s i) 0)))))))

(aux "world")
(aux "rkqodlw")

(defn scramble?
  "Функция возвращает true, если из букв в строке letters
  можно составить слово word."
  [letters word]

  (let [letters-dict (aux letters)
        word-dict (aux word)]
    (loop [i 0
           found true]
      (if (= i (count word)) found
          (recur (+ i 1) (and found (<= (get word-dict (nth word i) 0) (get letters-dict (nth word i) 0))))))))

; (scramble? "rkqodlw" "world")
; (scramble? "katas" "steak")
