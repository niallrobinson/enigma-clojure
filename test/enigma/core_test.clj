(ns enigma.core-test
  (:require [clojure.test :refer :all]
            [enigma.core :refer :all])
  (:use [debux core]))

(deftest test-codec
  (is (=
      (codec (:I rotors) \A)
      \E))
  )

(deftest test-rotate
  (is (=
      (codec (rotate-rotor (:I rotors)) \A)
      \J))
  )

(deftest test-plugboard
  (is (= 
        (plug plugboard \A)
        \B))
  (is (= 
        (plug plugboard \Z)
        \Z))
)

(deftest test-flip-rotor
  (is (= 
        (let [r (:I rotors)]
          (->> \A
              (codec r)
              (codec (flip-rotor r))
          )
        )
        \A
)))

(deftest test-one-rotors-pass
  (is (=
        (rotors-encode [(:III rotors)
                        (:II rotors)
                        (:I rotors)] \A)
        \Z))
(is (=
      (rotors-encode (rotate-rotors [(:III rotors)
                                     (:II rotors)
                                     (:I rotors)]) \H)
      \O))
)

(deftest test-encode-letter
  (testing "Encoding a letter")
  (is (=
        (encode-letter
          (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
          (:B reflectors)
          plugboard
          \H)
        \X
        ))
)
; ;   (is (=
; ;         (encode-letter
; ;           [(:III rotors) (:II rotors) (:I rotors)]
; ;           (:B reflectors)
; ;           plugboard
; ;           \H)
; ;         \O
; ;         ))
;   (is (=
;         (encode-letter
;           (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
;           (:B reflectors)
;           plugboard
;           \H) ;H G O M O M C P X X
;         \X
;         )))
; ;   (is (=
; ;       (encode-letter
; ;         (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
; ;         (:B reflectors)
; ;         plugboard
; ;         \T)
; ;       \O
; ;       ))
; ; )

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