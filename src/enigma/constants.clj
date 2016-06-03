(ns enigma.constants
  (:use [enigma.utils]))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def raw-alphabet (seq alphabet))

(def rotors {
  :I {:letters-out (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\V} :name "I"},
  :II {:letters-out (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\E} :name "II"},
  :III {:letters-out (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\Q} :name "III"},
  :IV {:letters-out (seq "ESOVPZJAYQUIRHXLNFTGKDCMWB") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\K} :name "IV"},
  :V {:letters-out (seq "VZBRGITYUPSDNHLXAWMJQOFECK") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\A} :name "V"},
  :VI {:letters-out (seq "JPGVOUMFYQBENHZRDKASXLICTW") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\A \N} :name "VI"},
  :VII {:letters-out (seq "NZJHGRCXMYSWBOUFAIVLPEKQDT") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\A \N} :name "VII"},
  :VIII {:letters-out (seq "FKQHTLXOCBJSPDZRAMEWNIUYGV") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\A \N} :name "VII"},
  :β {:letters-out (seq "LEYJVCNIXWPBQMDRTAKZGFUHOS") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{} :name "β"},
  :γ {:letters-out (seq "FSOKANUERHMBTIYCWLQPZXVGJD") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{} :name "γ"}
  })

(def reflectors {
    :B (with-meta (kv-map raw-alphabet (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT")) {:name "B"})
    :C (with-meta (kv-map raw-alphabet (seq "FVPJIAOYEDRZXWGCTKUQSBNMHL")) {:name "C"})
    :BDünn (with-meta (kv-map raw-alphabet (seq "ENKQAUYWJICOPBLMDXZVFTHRGS")) {:name "B Dünn"})
    :CDünn (with-meta (kv-map raw-alphabet (seq "RDOBJNTKVEHMLFCWZAXGYIPSUQ")) {:name "C Dünn"})
    })

(def plugboard {:A \B, :B \A, :C \D, :D \C, :E \F, :F \E,  :G \H, :H \G})