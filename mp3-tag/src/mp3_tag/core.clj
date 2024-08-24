(ns mp3-tag.core
  (:require
   [clojure.java.io :as io])
  (:import [java.io FileInputStream BufferedInputStream])
  (:import [java.nio.charset Charset]))

(defn read-binary-file [file-path]
  (with-open [file (FileInputStream. file-path)
              buffer (BufferedInputStream. file)]
    (let [file-size (.available file)
          byte-array (byte-array file-size)]
      (.read buffer byte-array)
      byte-array)))

(defn bytes-to-int [b1 b2 b3 b4]
  (bit-or
   (bit-shift-left b1 21)
   (bit-shift-left b2 14)
   (bit-shift-left b3 7)
   b4))

(def file-path "./k.mp3")
(def binary-data (read-binary-file file-path))

(defn decode-synchsafe [bytes]
  (reduce (fn [acc byte]
            (+ (bit-shift-left acc 7)
               (bit-and byte 0x7F)))
          0
          bytes))

(defn change-order [[b1 b2 & rest]]
  (if (nil? rest) (conj rest b1 b2) (conj (change-order rest) b1 b2)))

(defn bytes-to-utf16-string [byte-vector]
  (let [byte-array (byte-array byte-vector)
        utf16-charset (Charset/forName "UTF-16")]
    (String. byte-array utf16-charset)))

(bytes-to-utf16-string [0x1F, 0x04, 0x40, 0x04, 0x38, 0x04, 0x32, 0x04, 0x35, 0x04, 0x42, 0x04, 0x20, 0x00, 0x3C, 0x04, 0x38, 0x04, 0x40, 0x04, 0x21, 0x00])

(defn to-utf16-with-bom [[b1 b2 & rest]]
  (if (and (= b1 0xfe) (= b2 0xff))
    (bytes-to-utf16-string rest)
    (bytes-to-utf16-string (change-order rest))))

(defn bytes-to-utf8-string [byte-vector]
  (String. (byte-array byte-vector) "UTF-8"))

(defn parse-content [enc buffer]
  (cond
    (= enc 0x00) (apply str (map char buffer))
    (= enc 0x01) (to-utf16-with-bom buffer)
    (= enc 0x03) (bytes-to-utf8-string buffer)))

(defn dispatch-frame [frame]
  (println "print" (:id frame))
  (:id frame))

(defmulti decode-frame dispatch-frame)

(defmethod decode-frame "TALB" [frame]
  (merge frame {:content (str "Album: " (:content frame))}))

(defmethod decode-frame "TPE1" [frame]
  (merge frame {:content (str "Artist: " (:content frame))}))

(defmethod decode-frame "TIT2" [frame]
  (merge frame {:content (str "Songname: " (:content frame))}))

(defmethod decode-frame "TYER" [frame]
  (merge frame {:content (str "Year: " (:content frame))}))

(defmethod decode-frame "TCON" [frame]
  (merge frame {:content (str "Genre " (:content frame))}))

(defmethod decode-frame :default [frame]
  frame)

(defn read-frame [buffer total-size]
  (let [[id  r1] (split-at 4 buffer)
        [size-v r2] (split-at 4 r1)
        size (decode-synchsafe size-v)
        [flags r3] (split-at 2 r2)
        [enc r4] (split-at 1 r3)
        content (parse-content (first enc) (take (- size 1) r4))]
    {:id (apply str (map char id))
     :size size
     :flags flags
     :enc enc
     :content content
     :rest (drop size r3)
     :rest-size (- total-size size 10)}

    {:id (apply str (map char id))
     :size size
     :flags flags
     :enc enc
     :content content
     :rest (drop size r3)
     :rest-size (- total-size size 10)}))

(defn read-frames [b size]
  (loop [buffer b
         total size
         frames []]
    (let [{rest :rest rest-size :rest-size content :content id :id enc :enc :as frame} (read-frame buffer total)
          [b1 b2 b3 b4] rest
          decoded-frame (decode-frame frame)]
      (if (or (= 0x00 b1 b2 b3 b4) (<= rest-size 0))
        (conj frames decoded-frame)
        (recur rest rest-size (conj frames decoded-frame))))))

(let
 [[b1 b2 b3 & rest] binary-data
  marker (apply str (map char [b1 b2 b3]))
  [v1 v2 flags l1 l2 l3 l4 & rest1] rest
  has-extended (not= 0 (bit-and flags 0x40))
  has-unsync (not= 0 (bit-and flags 0x80))
  has-footer (not= 0 (bit-and flags 0x20))
  size (decode-synchsafe [l1 l2 l3 l4])
  [e1 e2 e3 e4 & rest2] rest1
  extended-size (if has-extended (bytes-to-int e1 e2 e3 e4) 0)
  r (drop extended-size rest1)
  t-size (- size 10 extended-size)
   ; frame1 (read-frame r t-size)
   ; frame2 (read-frame (:rest frame1) (:rest-size frame1))
   ; frame3 (read-frame (:rest frame2) (:rest-size frame2))
   ; frame4 (read-frame (:rest frame3) (:rest-size frame3))
   ; frame5 (read-frame (:rest frame4) (:rest-size frame4))
   ; frame6 (read-frame (:rest frame5) (:rest-size frame5))
   ; frame7 (read-frame (:rest frame6) (:rest-size frame6))
   ; frame8 (read-frame (:rest frame7) (:rest-size frame7))
  ]
  (map #(dissoc % :rest) (read-frames r t-size)))


