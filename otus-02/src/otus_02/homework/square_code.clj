(ns otus-02.homework.square-code
  (:require
   [clojure.string :as str]))

;; Реализовать классический метод составления секретных сообщений, называемый `square code`.
;; Выведите закодированную версию полученного текста.

;; Во-первых, текст нормализуется: из текста удаляются пробелы и знаки препинания,
;; также текст переводится в нижний регистр.
;; Затем нормализованные символы разбиваются на строки.
;; Эти строки можно рассматривать как образующие прямоугольник при печати их друг под другом.

;; Например,
"If man was meant to stay on the ground, god would have given us roots."
;; нормализуется в строку:

(def s "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots")
;; Разбиваем текст в виде прямоугольника.
;; Размер прямоугольника (rows, cols) должен определяться длиной сообщения,
;; так что c >= r и c - r <= 1, где c — количество столбцов, а r — количество строк.
;; Наш нормализованный текст имеет длину 54 символа
;; и представляет собой прямоугольник с c = 8 и r = 7:
"ifmanwas"
"meanttos"
"tayonthe"
"groundgo"
"dwouldha"
"vegivenu"
"sroots  "

(defn remove-spaces [s]
  (apply str (remove #(= % \space) s)))

(defn normalize-text [text]
  (-> text
      (clojure.string/replace #"\s+|\p{Punct}" "")
      clojure.string/lower-case))

(defn encode-string [input]
  (let [s (normalize-text input)
        n (+ 1 (int (Math/floor (Math/sqrt (double (count s))))))
        rows (partition n n nil s)
        transposed (map (fn [i] (map #(nth % i \space) rows)) (range n))
        res3 (clojure.string/join " " (map (fn [elem] (apply str (map str elem))) transposed))]
    res3))

(defn decode-string [s]
  (let [a (remove clojure.string/blank? (clojure.string/split s #" "))
        n (count (first a))
        transposed (map (fn [i] (map #(nth % i \space) a)) (range n))
        res (remove-spaces (apply str (map (fn [elem] (apply str (map str elem))) transposed)))]
    res))

(decode-string "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau ")

;; Закодированное сообщение получается путем чтения столбцов слева направо.
;; Сообщение выше закодировано как:
"imtgdvsfearwermayoogoanouuiontnnlvtwttddesaohghnsseoau"

;; Полученный закодированный текст разбиваем кусками, которые заполняют идеальные прямоугольники (r X c),
;; с кусочками c длины r, разделенными пробелами.
;; Для фраз, которые на n символов меньше идеального прямоугольника,
;; дополните каждый из последних n фрагментов одним пробелом в конце.
"imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "

;; Обратите внимание, что если бы мы сложили их,
;; мы могли бы визуально декодировать зашифрованный текст обратно в исходное сообщение:

"imtgdvs"
"fearwer"
"mayoogo"
"anouuio"
"ntnnlvt"
"wttddes"
"aohghn "
"sseoau "


