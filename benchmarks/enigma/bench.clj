(ns enigma.bench
  (:use perforate.core)
  (:require [enigma.core :refer :all]))

(defn uncrackable-no-args
  []
  (crack (vals (select-keys rotors [:I :II :III]))
  3
  (vals (select-keys reflectors [:B :C]))
  [plugboard]
  "XKHCELGHAX"
  "FEILHITLER"))

(defn crackable-no-args
  []
  (crack (vals (select-keys rotors [:I :II :III :IV :V :VI :VII :VIII :V :β :γ]))
  3
  (vals (select-keys reflectors [:B :C :BDünn :CDünn]))
  [plugboard]
  "XKHCELGHAX"
  "HEILHITLER"))

(defgoal crack-bench
  "Attempt to decode with 2880 solutions i.e. 3 from 10 rotors, 4 reflectors and a static plugboard")

(defcase crack-bench :default
  []
  uncrackable-no-args
)

; (defcase crack-bench :default
;   []
;   crackable-no-args
; )