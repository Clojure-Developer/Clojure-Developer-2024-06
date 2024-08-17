(ns url-shortener.core-test
  (:require [clojure.test :refer [are deftest is testing use-fixtures]]
            [matcher-combinators.test]
            [url-shortener.core :refer
             [alphabet-size id->int int->id list-all shorten! url-for]]))

(defn reset-state
  [t]
  (with-redefs [url-shortener.core/*counter (atom 0)
                url-shortener.core/*mapping (ref {})]
    (t)))

(use-fixtures :once reset-state)

(deftest test-int->id
  (are [input expected] (= (int->id input) expected)
    0 "0"
    alphabet-size "10"
    9999999999999 "2q3Rktod"
    9223372036854775807 "AzL8n0Y58W7"
    -1 "")
  (is (thrown-with-msg? Exception
                        #"cannot be cast to class java.lang.Number"
                        (int->id "asdf")))
  (is (thrown-with-msg? Exception #"because [\"]x[\"] is null" (int->id nil))))

(deftest test-id->int
  (are [input expected] (= (id->int input) expected)
    "0"       0
    "z"       61
    "clj"     149031
    "Clojure" 725410830262
    ""        0))

(deftest core
  (testing "Empty list" (is (= (list-all) [])))
  (testing "Shorten positive"
    (are [args result] (= (apply shorten! args) result)
      ["http://clojurebook.com"] "1"
      ["http://clojurebook.com"] "2"
      ["https://clojure.org" "clj"] "clj"
      ["https://clojure.org" "clj"] nil
      ["http://id-already-exists.com" "clj"] nil
      ["https://clojurescript.org" "cljs"] "cljs"
      [""] "3"))
  (testing "Getting urls"
    (are [id result] (= (url-for id) result)
      "1"    "http://clojurebook.com"
      "2"    "http://clojurebook.com"
      "3"    ""
      "cljs" "https://clojurescript.org"
      "clj"  "https://clojure.org"))
  (testing "List all"
    (is (match? (list-all)
                [{:id "1", :url "http://clojurebook.com"}
                 {:id "2", :url "http://clojurebook.com"}
                 {:id "clj", :url "https://clojure.org"}
                 {:id "cljs", :url "https://clojurescript.org"}
                 {:id "3", :url ""}]))))
