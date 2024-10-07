(ns otus-10.homework
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [otus-10.id3 :as id3])
  (:gen-class))

(def cli-options
  [["-f" "--filename FILE" "Filename" :validate [#(.exists (io/as-file %))]]
   ["-a" "--show-album"  :id :TALB]
   ["-A" "--show-artist" :id :TPE1]
   ["-t" "--show-title" :id :TIT2]
   ["-y" "--show-year" :id :TYER]
   ["-g" "--show-genre" :id :TCON]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Show mp3 info"
        ""
        "Usage: program-name [options]"
        ""
        "Options:"
        options-summary
        ""
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{opts :options, errors :errors summary :summary} (parse-opts args cli-options)]
    (if errors
      (apply println errors)
      (let [what-to-show (set (keys (dissoc opts :filename)))
            frames (:frames (id3/get-id3-header (:filename opts)))]
        (if (empty? what-to-show)
          (print (usage summary))
          (dorun (map  #(println (string/join " - " %))
                       (for [frame frames
                             :when (contains? what-to-show (:id frame))]
                         [(:title frame "None") (:body frame)]))))))))
