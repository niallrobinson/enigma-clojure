(ns enigma.core-test
  (:require [clojure.test :refer :all]
            [enigma.core :refer :all])
  (:use [debux core]))

(deftest test-char->int
  (testing "Char as int in the alphabet")
  (is (= (char->int \A) 0))
  (is (= (char->int \Z) 25))
  )

(deftest test-rotate-rotor
  (testing "Update the rotor key with the rotation")
  (is (= 
        (:rotation (meta (rotate-rotor (:I rotors))))
        1
    ))
  (is (= 
        (:rotation (meta (rotate-rotor (:I rotors) 28)))
        2
    ))
  )

(deftest test-rotate-rotors
  (testing "Sequence of rotors and nudges")
  (is (= (let [rotated-rotors (rotate-rotors [(:I rotors)
                                              (:II rotors)
                                              (:III rotors)])]
           (map #(:rotation (meta %)) rotated-rotors) ; get positions back
         )
       [1 0 0]
      )
  )
  (is (= (let [rotated-rotors (rotate-rotors [(rotate-rotor (:I rotors) 23)
                                              (:II rotors)
                                              (:III rotors)])]
           (map #(:rotation (meta %)) rotated-rotors)
         )
       [24 1 0]
      )
  )
  (is (= (let [rotated-rotors (rotate-rotors [(rotate-rotor (:I rotors) 23)
                                                (rotate-rotor (:II rotors) 22)
                                                (:III rotors)])]
           (map #(:rotation (meta %)) rotated-rotors)
         )
       [24 23 1]
      )
  )
  (is (= (let [rotated-rotors (rotate-rotors [(rotate-rotor (:I rotors) 23)
                                                (rotate-rotor (:II rotors) 22)
                                                (rotate-rotor (:III rotors) 17)])]
           (map #(:rotation (meta %)) rotated-rotors)
         )
       [24 23 18]
      )
  )
)

(deftest test-rotor-encode
  (testing "Make sure a rotor encodes a char to the correct letter")
  (is (=
        (rotor-encode (:I rotors) \B)
        \K
        )))

(deftest test-rotors-encode
  (testing "Right letter with multiple rotors")
  (is (=
        (rotors-encode (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)]) \G)
        \O
        )))

(deftest test-flipped-rotors-encode
  (testing "Right letter with multiple rotors but coming back the way")
  (is (=
        (rotors-encode (flip-rotors [(:III rotors) (:II rotors) (:I rotors)]) \G)
        \O
        )))


(deftest test-reflect
  (testing "reflect letter")
  (is (=
        (reflect (:B reflectors) \Y)
        )))


; (deftest test-encode-letter
;   (testing "Encoding a letter")
;   (is (=
;         (encode-letter
;           [(:III rotors) (:II rotors) (:I rotors)]
;           (:B reflectors)
;           plugboard
;           \H)
;         \X
;         ))
; )
;   (is (=
;         (encode-letter
;           [(:III rotors) (:II rotors) (:I rotors)]
;           (:B reflectors)
;           plugboard
;           \H)
;         \O
;         ))
;   (is (=
;         (encode-letter
;           (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
;           (:B reflectors)
;           plugboard
;           \H) ;H G O M O M C P X X
;         \X
;         ))
;     (is (=
;         (encode-letter
;           (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
;           (:B reflectors)
;           plugboard
;           \T)
;         \O
;         ))
; )

; (deftest test-encode-string
;   (testing "Encoding then decoding gives the same thing")
;   (let [encoded (encode-string
;                   [(:III rotors) (:II rotors) (:I rotors)]
;                   (:B reflectors)
;                   plugboard
;                   "HELLOWORLD")]
;   (is (= (encode-string
;                   [(:III rotors) (:II rotors) (:I rotors)]
;                   (:B reflectors)
;                   plugboard
;                   encoded)
;         "HELLOWORLD"
;         ))))