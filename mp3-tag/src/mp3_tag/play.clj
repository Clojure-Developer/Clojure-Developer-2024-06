(ns mp3-tag.play)

(defn print-all [& args]
  (println (first args) (type args)))

(print-all 1 2 34)

(defn dispatch-book [book]
  (cond
    (vector? book) :vector-book
    (contains? book :title) :standard-map
    (contains? book :book) :alternative-map))

(defmulti normalize-book dispatch-book)

(defmethod normalize-book :vector-book [book]
  {:title (first book) :author (second book)})

(defmethod normalize-book :standard-map [book]
  book)

(defmethod normalize-book :alternative-map
 [book]
 {:title (:book book) :author (:by book)}
 )


(normalize-book {:title "War and Peace" :author "Tolstoy"})
(normalize-book ["1984" "Orwell"])


(defn fact [n acc]
  (if (= n 1) 
    acc
    (recur (- n 1) (* n acc) )
  ))

(apply + [1 2 3])

(fact 300 1N)


(def the-answer (promise))

(doto (Thread. (fn []
                 (Thread/sleep 10000)
                 (deliver the-answer 42)))
  .start)

(deref the-answer)
























