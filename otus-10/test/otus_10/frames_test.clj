(ns otus-10.frames-test
  (:require [clojure.test :refer :all]
            [otus-10.frames :refer :all]))

(deftest test-valid-frame?
  (let [test-cases [{:id "ABCD" :expected true}
                    {:id "A2C4" :expected false}
                    {:id "A2C" :expected false}
                    {:id "ABCDE" :expected false}
                    {:id "A2C!" :expected false}
                    {:id 1234 :expected false}
                    {:id "" :expected false}
                    {:id "1234" :expected false}
                    {:id nil :expected false}]]
    (doseq [{:keys [id expected]} test-cases]
      (is (= (valid-frame? id) expected)
          (str "Test failed for id: " id)))))

(deftest decode-text-test
  (let [test-cases [{:id 0, :text [104 101 108 108 111], :expected "hello"}
                    {:id 1, :text [-2, -1, 0, 104, 0, 101, 0, 108, 0, 108, 0, 111], :expected "hello"}
                    {:id 2, :text [0, 104, 0, 101, 0, 108, 0, 108, 0, 111], :expected "hello"}
                    {:id 3, :text [104 101 108 108 111], :expected "hello"}
                    {:id 4, :text [104 101 108 108 111], :expected :exception}]]

    (doseq [{:keys [id text expected]} test-cases]
      (if (= expected :exception)
        (is (thrown? Exception (decode-text id text)))
        (is (= expected (decode-text id text)))))))
