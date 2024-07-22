(ns otus.homework-test
  (:require [clojure.test :refer :all]
            [otus.homework :as sut]))


(deftest s-expression-solution-test
  (is (= (sut/solution) -0.24666666666666667)
      "solution function returns a correct answer"))
