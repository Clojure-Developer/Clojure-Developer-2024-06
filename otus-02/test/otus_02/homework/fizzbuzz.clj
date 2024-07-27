(ns otus-02.homework.fizzbuzz
  (:require
   [clojure.test :refer :all]))

(defn is-divisible [n by]
  (zero? (mod n by)))

(defn fizz-buzz [n]
  "Создайте программу, которая выводит числа от 1 до n.
   - Если число делится на 3, выведите 'Fizz';
   - если число делится на 5, выведите 'Buzz';
   - если число делится и на 3 и на 5, выведите 'FizzBuzz'."
  (map (fn [e] (cond 
                 (is-divisible e 15) "FizzBuzz" 
                 (is-divisible e 5) "Buzz" 
                 (is-divisible e 3) "Fizz" :else e)
         ) (range 1 (+ 1 n))))

(fizz-buzz 10)

(deftest fizz-buzz-test
  (is (= (fizz-buzz 10)
         '(1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz"))))
