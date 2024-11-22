(ns otus-02.homework.pangram
  (:require [clojure.string :as string]
            [clojure.set :refer [difference]]
            [clojure.test :refer [is]]
            [otus-02.homework.lib :refer [normalize]]))

(def ^:private alpha (set "abcdefghijklmnopqrstuvwxyz"))

(defn is-pangram
  [test-string]
  (->> test-string
       string/lower-case
       set
       (difference alpha)
       empty?))

(defn is-pangram-0
  [^String test-string]
  (= 26
     (count (->> test-string
                 normalize
                 set))))

;; optimized for large text
(defn is-pangram-1
  [^String test-string]
  (loop [rest-alpha alpha
         rest-string (lazy-seq test-string)]
    (cond (empty? rest-alpha) true
          (empty? rest-string) false
          :else (recur (difference rest-alpha
                                   (set (string/lower-case (first
                                                             rest-string))))
                       (rest rest-string)))))

(comment
  (is-pangram "The quick brown fox jumps over the lazy dog")
  (is-pangram-0 "The quick brown fox jumps over the lazy dog")
  (is-pangram-1 "The quick brown fox jumps over the lazy dog")
  (doseq [f [is-pangram is-pangram-0 is-pangram-1]]
    (time
      (dotimes [_ 1000]
        (doseq
          [s ["The quick brown fox jumps over the lazy dog"
              "
abcdefghijklmnopqrstuvwxyz
Lorem ipsum dolor sit amet, consectetur adipiscing elit. In at sapien
nibh. Morbi condimentum gravida risus, quis interdum nunc efficitur
nec. Donec sollicitudin, nisi nec gravida gravida, nunc dolor tempor
odio, maximus molestie metus odio ut tellus. Proin semper ligula ut
est tempus congue. In et viverra dui. Nunc quis porttitor felis. In
imperdiet nisi ut urna vehicula malesuada. Aenean magna ex, egestas
eget massa id, fringilla consequat nunc. Integer a rutrum mauris. Proin
congue ut purus vitae finibus. Orci varius natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Maecenas porttitor augue
eget porta efficitur. Nunc sed sapien lectus. Maecenas ac mi dapibus,
porttitor nulla ut, vestibulum lorem. Donec id vehicula lectus.

Praesent suscipit, magna nec faucibus congue, leo libero rutrum arcu,
sodales aliquet urna odio tincidunt felis. Nulla aliquam hendrerit
nibh. Etiam quis tincidunt odio. Nunc ultricies, libero eget tincidunt
rutrum, massa dui viverra eros, ac convallis erat magna vitae metus. Fusce
vehicula leo lacus, quis luctus turpis dignissim et. Donec ultrices,
tortor vitae faucibus faucibus, lorem arcu euismod enim, vitae venenatis
nibh dui vitae elit. Ut sed faucibus leo, at ornare ex. Pellentesque
ac lobortis metus. Fusce nec justo vitae tellus porta congue quis
vel justo. Curabitur finibus lectus velit, in tempor nibh tincidunt
sed. Donec ex neque, lacinia a erat quis, feugiat aliquet mauris. Donec
laoreet ipsum ac dui tristique consequat. Proin porta eros ex, non dapibus
magna convallis eget. Nam varius imperdiet velit ac dignissim. Duis ac
mi sit amet sapien aliquam hendrerit ac sit amet enim. Nunc a urna a
arcu feugiat mattis.

Nunc quis suscipit elit. Praesent tincidunt, mauris ut auctor auctor,
urna sem eleifend ex, vitae feugiat nisl tortor id ligula. Suspendisse
potenti. Duis sodales tortor eu tortor posuere commodo. Duis non rhoncus
ligula, nec gravida quam. Mauris iaculis, justo eget dictum vestibulum,
lacus diam pharetra ipsum, eu auctor velit tortor quis libero. Integer
sit amet dolor dui. In et lectus ut mi blandit eleifend. Sed non mauris
massa. Nam vitae libero ac neque porttitor fringilla. Vestibulum pretium,
lorem sed accumsan fringilla, nibh lacus egestas urna, interdum dignissim
urna est et augue. Suspendisse eleifend volutpat enim, et maximus odio
accumsan vitae.

Proin egestas sapien a interdum molestie. Nulla dapibus pretium
accumsan. Proin nec felis elit. Sed at egestas ipsum. Fusce luctus
odio vitae magna eleifend convallis. Vivamus pulvinar lacus mauris,
lobortis lobortis leo volutpat id. Vivamus feugiat cursus ante, nec
posuere est posuere ac. In eleifend vel est nec faucibus. Sed dolor
lectus, molestie sed nisl dapibus, ullamcorper mollis libero. Nulla
in eros et justo aliquam varius eget sit amet justo. Aenean efficitur
massa vel aliquam vestibulum. In turpis massa, lacinia sed pharetra non,
fringilla quis leo. Pellentesque tincidunt accumsan congue. Praesent in
mauris volutpat, auctor urna quis, tincidunt mauris. Sed posuere gravida
ipsum ac fringilla.

Sed posuere a sem a tincidunt. Pellentesque lacinia pellentesque ligula eu
sagittis. Nam est arcu, egestas eu ipsum vel, lacinia rhoncus purus. Etiam
eleifend elit eget orci auctor posuere. Sed vel urna non risus mattis
gravida. Proin sodales justo eu congue placerat. Praesent posuere iaculis
laoreet. Praesent fermentum in augue nec ultrices. Aliquam pretium nibh
at nulla convallis commodo sed id ex. Cras sit amet eros ut purus accumsan
lacinia eget in risus. Cras vulputate enim arcu, eu fermentum leo aliquet
eu. Morbi auctor odio sit amet ullamcorper lobortis. Sed at nibh tempus,
lobortis enim nec, pretium enim. In ut dictum arcu.

Aenean sit amet lacus egestas, scelerisque lacus vitae, luctus
dolor. Donec eleifend nulla augue, et dignissim est commodo id. Morbi
porttitor, dui in feugiat tempor, augue eros viverra ligula, ac facilisis
orci odio sed eros. Nulla vestibulum feugiat dapibus. Curabitur eros
lorem, feugiat a neque lacinia, placerat maximus dui. Proin vitae
felis eleifend, dapibus lectus eu, hendrerit magna. Nunc sapien lorem,
rhoncus eu condimentum nec, placerat sit amet ligula. Nullam elit augue,
sodales quis aliquet eu, auctor in nisi. Sed pulvinar tincidunt felis,
nec tincidunt eros viverra sed. Pellentesque eu vulputate mi, eget
mattis nulla. Donec bibendum volutpat urna in scelerisque. Donec
tincidunt accumsan feugiat. Nunc fringilla odio ut metus maximus,
finibus elementum arcu hendrerit. Interdum et malesuada fames ac ante
ipsum primis in faucibus. Integer enim magna, porta in malesuada congue,
scelerisque ut turpis. Aenean dapibus non purus sed sagittis.

Aliquam erat volutpat. In sapien nulla, venenatis in facilisis at,
volutpat id orci. Nulla est libero, consequat gravida porta nec,
luctus nec lacus. Donec condimentum est et nisl fermentum auctor. Fusce
id bibendum sem, ac pellentesque sem. Pellentesque auctor sed sem ut
lacinia. Integer non risus vel diam bibendum tempus. Proin ipsum tortor,
ornare ut erat eu, pharetra pellentesque nisi. Donec rutrum ex mauris,
at consequat enim hendrerit in. Nunc lorem velit, condimentum ac commodo
non, placerat vel ante.

Phasellus sed ipsum dignissim, sodales felis eu, imperdiet
nunc. Vestibulum semper quam ac leo porta, quis hendrerit leo
auctor. Pellentesque malesuada tristique orci, eget fringilla erat gravida
ut. Pellentesque habitant morbi tristique senectus et netus et malesuada
fames ac turpis egestas. Nulla non mi arcu. Nullam venenatis odio eget
mauris pretium, id scelerisque diam aliquam. Quisque non felis ex. Donec
non augue porta, ultrices mi eu, accumsan tellus. Suspendisse arcu ligula,
accumsan non metus ut, semper posuere ipsum. Integer sollicitudin ante eu
massa cursus lacinia. Cras id placerat eros. In sed feugiat lectus. Proin
at eros vel dolor blandit hendrerit vel iaculis leo. Proin non quam sit
amet erat mattis pellentesque sit amet in magna.

Quisque vel scelerisque est. Donec ultrices condimentum pharetra. Aliquam
rutrum vel massa id maximus. Maecenas vel varius ligula. Praesent vitae
tellus hendrerit, porta lacus ac, auctor lectus. Phasellus a lacus sit
amet ligula rhoncus iaculis. Vestibulum at bibendum turpis. Aenean ac
volutpat metus, vitae pulvinar dolor. Ut a sagittis nulla. Curabitur in
ullamcorper orci. Cras eleifend et tortor eu efficitur. Mauris vitae
turpis ac velit tristique vestibulum in at urna. Aenean porta nunc eu
porttitor malesuada. Vestibulum tortor odio, hendrerit vitae felis ac,
elementum tincidunt tellus. Aliquam gravida quam finibus lorem cursus
pellentesque.
                       "
              "How vexingly quick daft zebras jump"
              "Waltz, bad nymph, for quick jigs vex"
              "Sphinx of black quartz, judge my vow"
              "Jackdaws love my big sphinx of quartz"]]
          (is (f s)))
        (doseq [s ["The quick brown fox jumps over the dog"
                   "Keep in mind that"]]
          (is (not (f s))))))))

;; Without Lorem ipsum
; (out) "Elapsed time: 43.664686 msecs"
; (out) "Elapsed time: 29.749669 msecs"
; (out) "Elapsed time: 52.491599 msecs"

;; With Lorem ipsum
; (out) "Elapsed time: 391.425171 msecs"
; (out) "Elapsed time: 475.359696 msecs"
; (out) "Elapsed time: 59.689839 msecs"

;; is-pangram-1 optimized for large texts.
