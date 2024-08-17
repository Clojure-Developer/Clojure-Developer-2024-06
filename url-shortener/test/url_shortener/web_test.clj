(ns url-shortener.web-test
  (:require [clojure.test :refer [are deftest testing use-fixtures]]
            [matcher-combinators.test]
            [url-shortener.core-test :refer [reset-state]]
            [url-shortener.web :refer [handler router]]))

(use-fixtures :once reset-state)

(deftest ^:integration test-handler
  (testing "Handlers"
    (are [request response] (match? (handler request) response)
      ; Post
      {:uri "/",
       :request-method :post,
       :params
         {:url
            "https://github.com/Clojure-Developer/Clojure-Developer-2022-10"}}
        {:status 201,
         :headers {"Location" "1",
                   "Content-Type" "application/json; charset=utf-8"},
         :body "{\"id\":\"1\"}"}
      ; Put clj
      {:uri "/clj", :request-method :put, :params {:url "https://clojure.org"}}
        {:status 201,
         :headers {"Location" "clj",
                   "Content-Type" "application/json; charset=utf-8"},
         :body "{\"id\":\"clj\"}"}
      ; Negative, duplicate
      {:uri "/clj", :request-method :put, :params {:url "https://clojure.org"}}
        {:status 409,
         :headers {"Content-Type" "application/json; charset=utf-8"},
         :body "{\"error\":\"Short URL clj is already taken\"}"}
      ; Get result
      {:uri "/clj", :request-method :get} {:status 302,
                                           :headers {"Location"
                                                       "https://clojure.org"},
                                           :body ""}))
  (testing "Routers"
    (are [request response] (match? (router request) response)
      ;  List
      {:uri "/list/", :request-method :get}
        {:status 200,
         :headers {},
         :body
           {:urls
              [{:id "1",
                :url
                  "https://github.com/Clojure-Developer/Clojure-Developer-2022-10"}
               {:id "clj", :url "https://clojure.org"}]}})))
