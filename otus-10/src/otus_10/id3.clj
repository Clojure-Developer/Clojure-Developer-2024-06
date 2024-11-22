(ns otus-10.id3
  (:require [clojure.java.io :as io]
            [otus-10.utils :as u]
            [otus-10.frames :as f]))

(defn valid-id3?
  "Checks if id3 is valid according to the pattern:
     $49 44 33 yy yy xx zz zz zz zz"
  [header]
  (let [[h1 h2 h3 yy1 yy2 _ zz1 zz2 zz3 zz4] header]
    (and (every? true? (map = [\I \D \3] [h1 h2 h3]))
         (every? #(< (int %) 0xff) [yy1 yy2])
         (every? #(< (int %) 0x80) [zz1 zz2 zz3 zz4]))))

(defn get-id3-flags
  "Returns map of base header flags."
  [header]
  (let [flag-bit (int (nth header 5))]
    (zipmap [:unsync :extended :exp :footer]
            [(bit-test flag-bit 7) (bit-test flag-bit 6) (bit-test flag-bit 5)
             (bit-test flag-bit 4)])))

(defn get-id3-header
  "Returns map with set header flags and frames."
  [filename]
  (with-open [r (io/reader filename)]
    (let [f (slurp r)
          header (take 10 f)
          size (u/bytes->num (map int (drop 6 header)))
          full-header (take size f)
          flags (get-id3-flags header)
          ext-header-size (if (:extended flags) (u/bytes->num (map int (take 4 (drop 10 f)))) 0)
          frames (drop (+ 10 ext-header-size) full-header)]
      (when (valid-id3? header)
        (assoc flags
               :header-size size
               :frames (f/frame-reader frames))))))

(comment
  (get-id3-header "resources/file-12926-ed090b.mp3")
  (get-id3-header "resources/sample.mp3"))
