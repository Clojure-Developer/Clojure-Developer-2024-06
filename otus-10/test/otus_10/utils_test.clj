(ns otus-10.utils-test
  (:require [clojure.test :refer :all]
            [otus-10.utils :refer :all]))

(deftest test-bytes->num
  (are [expected input] (= expected (bytes->num input))
    ;; Test cases
    0 [0]
    255 [255]
    256 [1 0]
    65535 [255 255]
    16777215 [255 255 255]
    4294967295 [255 255 255 255]
    1 [0 0 0 1]
    258 [0 1 2]))
