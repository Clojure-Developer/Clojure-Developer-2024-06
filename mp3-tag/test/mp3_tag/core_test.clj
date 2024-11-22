(ns mp3-tag.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [mp3-tag.core :refer :all])
  )



(def little-hello [0xFF, 0xFE, 0x1F, 0x04, 0x40, 0x04, 0x38, 0x04, 0x32, 0x04, 0x35, 0x04, 0x42, 0x04, 0x20, 0x00, 0x3C, 0x04, 0x38, 0x04, 0x40, 0x04, 0x21, 0x00])

(def big-hello [0xFE 0xFF 0x04 0x1f 0x04 0x40 0x04 0x38 0x04 0x32 0x04 0x35 0x04 0x42 0x00 0x20 0x04 0x3c 0x04 0x38 0x04 0x40 0x00 0x21])

(def hello [0xd0, 0x9f, 0xd1, 0x80, 0xd0, 0xb8, 0xd0, 0xb2, 0xd0, 0xb5, 0xd1, 0x82, 0x20, 0xd0, 0xbc, 0xd0, 0xb8, 0xd1, 0x80, 0x21])

(def talb-frame {:content "Michael Jackson" :id "TPE1" })
(def tit2-frame {:content "Yersterday" :id "TIT2" })
(def tyer-frame {:content "2014" :id "TYER" })
(def tcon-frame {:content "Rock" :id "TCON" })




(deftest a-test
  (testing "syncsafe decode"
    (is (= (decode-synchsafe [0x00 0x00]) 0))
    (is (= (decode-synchsafe [0x00 0xC0]) 64))
    (is (= (decode-synchsafe [0x01 0xff]) 255)))

  (testing "change order of bytes"
    (is (= (change-order [0x00 0x00]) [0x00 0x00]))
    (is (= (change-order  [0x01 0x02]) [0x02 0x01])))

  (testing "string encodings"
    (is (= (change-order [0x00 0x00]) [0x00 0x00]))
    (is (= (change-order  [0x01 0x02]) [0x02 0x01]))

    (is (= "Привет мир!" (to-utf16-with-bom little-hello))

        (is (= "Привет мир!" (to-utf16-with-bom
                              big-hello))

            (is (= "Привет мир!" (bytes-to-utf8-string hello)))))
    )
  
  (testing "parse content"
    
    (is (= "123" (parse-content 0 [0x31 0x32 0x33]) ))
    (is (= "Привет мир!" (parse-content 1 little-hello) ))
    
    (is (= "Привет мир!" (parse-content 1 big-hello)))
    (is (= "Привет мир!" (parse-content 3 hello)))
  )

  (testing "decode-frame"
    (is (str/includes? (:content (decode-frame talb-frame)) "Artist"))
    )
 
  (testing "decode-frame"
    (is (str/includes? (:content (decode-frame tit2-frame)) "Songname"))
    )

  (testing "decode-frame"
    (is (str/includes? (:content (decode-frame tyer-frame)) "Year"))
    )

  (testing "decode-frame"
    (is (str/includes? (:content (decode-frame tcon-frame)) "Genre"))
    )

  )




