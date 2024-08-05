(ns url-shortener.frontend-test
  (:require [clojure.test :refer [deftest is]]
            [url-shortener.frontend :refer [header]]))

; pretty useless I think
(deftest test-header
  (is (= (header "Test") [:h2 {:style {:margin "8px 4px"}} "Test"])))
