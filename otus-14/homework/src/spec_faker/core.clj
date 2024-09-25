(ns spec-faker.core
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer :all]
            [hiccup.core :refer [html]]
            [hiccup.form :refer [form-to label submit-button text-area]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [cheshire.core :as json]
            [ring.middleware.reload :refer [wrap-reload]]
            [clojure.spec.gen.alpha :as gen]
            [ring.swagger.swagger2 :as rs]
            [ring.swagger.swagger-ui :as swagger-ui]
            [schema.core :as ss]
            [clojure.spec.alpha :as s])
  (:gen-class))

(def sw
  (ss/with-fn-validation
    (rs/swagger-json
     {:info {:version "1.0.0"
             :title "Fake json generator"
             :description "Generation of json on spec"}
      :paths {"/" {:get {:parameters {:query {:spec ss/Str}}
                         :responses {200 {}}}

                   :post {:description "Generation of json"
                          :tags ["spec"]
                          :parameters {:query {:spec ss/Str}
                                       :body {}}
                          :responses {200 {}
                                      404 {:description "Ohnoes."}}}}}})))

(spit "resources/public/swagger.json" (json/generate-string sw))

(defn to-spec [m]
  (let [value-type (get m "type")
        sp (case value-type
             "string" (s/def spec-name string?)
             "number" (s/def spec-name number?)
             "boolean" (s/def spec-name boolean?)
             "integer" (s/def spec-name pos-int?))]
    (gen/generate (s/gen sp))))

(defn gen [properties]
  (->> (keys properties)
       (reduce (fn [acc k] (assoc acc k (to-spec (get properties k)))) {})
       json/generate-string))

(def valid-types #{"string" "number" "boolean" "integer"})

(defn is-valid-spec [spec]
  (let [fields-object (get-in spec ["properties"])
        fields (keys fields-object)
        types (map  (fn [field] (get-in fields-object [field, "type"])) fields)
        are-types-valid (every? (fn [e] (contains? valid-types e)) types)]
    are-types-valid))

(defn render-form []
  (html
   [:head
    [:link {:rel "stylesheet" :href "style.css"}]]
   [:h1 "Enter spec"]
   (form-to [:get "/?spec"]
            [:div {:class "flex flex-col"}
             (label "spec" "Spec:")
             (text-area {:rows 20} "spec")
             (submit-button "Submit")])))

(defn render-gen [parsed-spec]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (gen (get-in parsed-spec ["properties"]))})

(defn render-bad []
  {:status 400
   :body "Bad request"})

(defroutes app-routes
  (GET "/" request
    (let [params (:query-params request)
          spec (get-in params ["spec"])
          parsed-spec (json/parse-string spec)]
      (println parsed-spec)
      (println (is-valid-spec parsed-spec))

      (if spec (if (is-valid-spec parsed-spec) (render-gen parsed-spec) (render-bad))
          (render-form))))

  (route/not-found "Page not found"))

(def app
  (-> app-routes
      (wrap-reload)
      wrap-params
      (swagger-ui/wrap-swagger-ui {:path "/swagger-ui"
                                   :url "/api/swagger.json"})
      (wrap-resource "public")))

(defn init []
  (println "Server is starting..."))

(defn destroy []
  (println "Server is shutting down..."))

(defn -main [& args]
  (run-jetty app {:port 8000}))

