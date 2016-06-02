(ns enigma.bench
  (:use perforate.core)
  (:require [enigma.core :refer :all]))


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
  "Test the steps for encoding")

(defcase encode :cyphur
  "One complete cyphur"
  []
  (encode-string 
      (vals (select-keys rotors [:I :II :III]))
      (:B reflectors)
      plugboard
      "HEILHILTER"))

(defcase encode :rotate
  "One rotation"
  []
  (rotate-rotors (vals (select-keys rotors [:I :II :III]))))

; (defcase crack-bench :default
;   []
;   crackable-no-args
; )