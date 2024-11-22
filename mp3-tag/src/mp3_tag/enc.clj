(ns mp3-tag.enc
  (:import [java.nio.charset Charset])
  )

(defn change-endianness [bytes]
  (vec (reverse bytes)))

(defn change-order [[b1 b2 & rest]]
  (if (nil? rest) (conj rest b1 b2) (conj (change-order rest) b1 b2))
  )

(defn bytes-to-utf16-string [byte-vector]
  (let [byte-array (byte-array byte-vector)
        utf16-charset (Charset/forName "UTF-16")]
    (String. byte-array utf16-charset)))


(defn to-utf16-with-bom [[b1 b2 & rest]]
  (if (and (= b1 0xfe) (= b2 0xff))
   (bytes-to-utf16-string rest)
   (bytes-to-utf16-string (change-order rest))
   )
  )



(map #(format "%02x" (bit-and % 0xff)) (change-order [0x39 0x00 0x31 0x00])  )


(to-utf16-with-bom [0xff 0xfe 0x39 0x00 0x39 0x00])
(to-utf16-with-bom [0xfe 0xff 0x00 0x39 0x00 0x39])
(to-utf16-with-bom [0xff 0xfe 0x1f 0x04 0x40 0x04 0x38 0x04 0x32 0x04 0x35 0x04 0x42 0x04])

; (map #(format "%02x" (bit-and % 0xff)) (to-utf16-with-bom [0xff 0xfe 0x39 0x00 0x39 0x00]) )

;; Usage
(def byte-vector [0x00 0x48 0x00 0x65 0x00 0x6C 0x00 0x6C 0x00 0x6F]) ; Represents "Hello" in UTF-16
(def utf16-string (bytes-to-utf16-string byte-vector))

(bytes-to-utf16-string byte-vector)



(map #(format "%02x" (bit-and % 0xff)) (change-endianness [0x39 0x00 0x39 0x00]))


(def byte-vector2 [0x00 0x43 0x00 0x4f 0x00 0x1f 0x04 0x40 0x04 0x38 0x04 0x32 0x04 0x35]) ; "Hello" in UTF-16
(def byte-vector2 [0x39 0x00]) ; "Hello" in UTF-16
(bytes-to-utf16-string byte-vector2)


(bytes-to-utf16-string [0x00 0x39])

(println utf16-string) ; Output



(defn bytes-to-utf8-string [byte-vector]
  (String. (byte-array byte-vector) "UTF-8"))

(bytes-to-utf8-string [0x6d 0x61 0x6a 0x6f 0x72 0x5f])





















