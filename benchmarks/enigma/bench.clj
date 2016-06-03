(ns enigma.bench
  (:use perforate.core)
  (:require [enigma.core :refer :all]
            [enigma.rotorops :refer :all]
            [enigma.utils :refer :all]
            [enigma.constants :refer :all]
            [enigma.encode :refer :all]
            [enigma.codecs :refer :all]))


; (defgoal crack-bench
;   "Attempt to decode with 2880 solutions i.e. 3 from 10 rotors, 4 reflectors and a static plugboard")

; (defcase crack-bench :default
;   []
;   (crack (vals (select-keys rotors [:I :II :III]))
;   3
;   (vals (select-keys reflectors [:B :C]))
;   [plugboard]
;   "XKHCELGHAX"
;   "FEILHITLER"))

(defgoal encode
  "Test the steps for encoding"
  :setup (fn []
          [(vals (select-keys rotors [:I :II :III]))
           (:I rotors)]))

(defcase encode :encode-pass
  "One complete cyphur"
  [r rs]
  (encode-string 
      (vals (select-keys rotors [:I :II :III]))
      (:B reflectors)
      plugboard
      "HEILHILTER"))

(defcase encode :rotate
  "One rotation"
  [r rs]
  (rotate-rotors rs))

(defcase encode :rotor-encode
  "One rotation"
  [r rs]
  (codec r "A"))

(defcase encode :map-and-get
  [r rs]
  (:A (kv-map (:alphabet r) (:letters-out r))))

; (defcase crack-bench :default
;   []
;   crackable-no-args
; )