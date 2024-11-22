(ns otus-10.id3-test
  (:require [clojure.test :refer :all]
            [otus-10.id3 :refer :all]))

(deftest test-valid-id3?
  (let [test-cases [{:input [\I \D \3 0x00 0x00 nil 0x00 0x00 0x00 0x00] :expected true}
                    {:input [\I \D \3 0x7F 0x7F nil 0x7F 0x7F 0x7F 0x7F] :expected true}
                    {:input [\I \D \2 0x00 0x00 nil 0x00 0x00 0x00 0x00] :expected false}
                    {:input [\I \D \3 0x80 0x00 nil 0x00 0x00 0x00 0x00] :expected true}
                    {:input [\I \D \3 0x00 0x00 nil 0x79 0x00 0x00 0x00] :expected true}
                    {:input [nil \D \3 0x00 0x00 nil 0x00 0x00 0x00 0x00] :expected false}]]
    (doseq [{:keys [input expected]} test-cases]
      (is (= expected (valid-id3? input))))))

(deftest get-id3-flags-test
  (let [test-cases [{:header [0 0 0 0 0 128] :expected {:unsync true :extended false :exp false :footer false}}
                    {:header [0 0 0 0 0 64]  :expected {:unsync false :extended true :exp false :footer false}}
                    {:header [0 0 0 0 0 32]  :expected {:unsync false :extended false :exp true :footer false}}
                    {:header [0 0 0 0 0 16]  :expected {:unsync false :extended false :exp false :footer true}}
                    {:header [0 0 0 0 0 240] :expected {:unsync true :extended true :exp true :footer true}}
                    {:header [0 0 0 0 0 0]   :expected {:unsync false :extended false :exp false :footer false}}]]
    (doseq [{:keys [header expected]} test-cases]
      (is (= expected (get-id3-flags header))))))
