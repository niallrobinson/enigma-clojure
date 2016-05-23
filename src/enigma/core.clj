(ns enigma.core
  (:gen-class)
  (:use [debux core]))

(defn -main
  [& args]
  (println "Hello, World!"))

(def rotors {
    :I (with-meta (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ") {:nudge \R :rotation 0}),
    :II (with-meta (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE") {:nudge \F :rotation 0}),
    :III (with-meta (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO") {:nudge \W :rotation 0});,
    ; "IV"    : { mapper: "ESOVPZJAYQUIRHXLNFTGKDCMWB", step: "K"},
    ; "V"     : { mapper: "VZBRGITYUPSDNHLXAWMJQOFECK", step: "A"},
    ; "VI"    : { mapper: "JPGVOUMFYQBENHZRDKASXLICTW", step: "AN"},
    ; "VII"   : { mapper: "NZJHGRCXMYSWBOUFAIVLPEKQDT", step: "AN"},
    ; "VIII"  : { mapper: "FKQHTLXOCBJSPDZRAMEWNIUYGV", step: "AN"},
    ; "β"     : { mapper: "LEYJVCNIXWPBQMDRTAKZGFUHOS", step: ""},
    ; "γ"     : { mapper: "FSOKANUERHMBTIYCWLQPZXVGJD", step: ""}
  })

(def reflectors {
    :B {:A "Y", :B "R", :C "U", :D "H", :E "Q", :F "S", :G "L", :I "P", :J "X", :K "N", :M "O", :T "Z", :V "W"};,
    ; "C":    ['AF', 'BV', 'CP', 'DJ', 'EI', 'GO', 'HY', 'KR', 'LZ', 'MX', 'NW', 'TQ', 'SU'],
    ; "B Dünn":   ['AE', 'BN', 'CK', 'DQ', 'FU', 'GY', 'HW', 'IJ', 'LO', 'MP', 'RX', 'SZ', 'TV'],
    ; "C Dünn":   ['AR', 'BD', 'CO', 'EJ', 'FN', 'GT', 'HK', 'IV', 'LM', 'PW', 'QZ', 'SX', 'UY']
  })

(defn rotate-rotor
  ([rotor]
   (rotate-rotor rotor 1))
  ([rotor nsteps]
   (let [rotation (:rotation (meta rotor))]
    (vary-meta rotor assoc :rotation (mod (+ rotation nsteps) (count rotor)))
   )
  )
)

(defn nudge?
  [rotor]
  (= 
    (nth rotor (:rotation (meta rotor)))
    (:nudge (meta rotor))
    )
  )

(defn rotate-rotors
  ([rotors]
    (rotate-rotors (first rotors) (rest rotors) []))
  ([rotor rotors rotated-rotors]
    (let [updated-rotors (conj rotated-rotors (rotate-rotor rotor))]
      (if (and (seq rotors) (nudge? rotor))
        (rotate-rotors (first rotors) (rest rotors) updated-rotors)
        (into updated-rotors rotors)
        )
    )
  )
)

(defn char->int
  [c]
  (- (int c) 65) ;to get a->1, b->2 etc
  )

(defn rotor-encode
  [rotor letter]
  (nth rotor (mod (+ (:rotation (meta rotor)) (char->int letter)) (count rotor)))
  )

(defn rotors-encode
  ([rotors letter]
    (rotors-encode (first rotors) (rest rotors) letter))
  ([rotor rotors letter]
   (let [nextletter (rotor-encode rotor letter)]
     (if (empty? rotors)
      nextletter
      (recur (first rotors) (rest rotors) nextletter))))
  )

(defn reflect
  [reflector letter])

(defn encode-letter 
  [rotors reflector letter]
    (-> letter
        (partial rotors-encode rotors)
        (partial reflect reflector)
        (partial rotors-encode (reverse rotors)))
  )

; (defn rotate
;   ([rotors]
;    (rotate (first rotors) (rest rotors)) [])
;   ([rotor rotors rotated-rotors]
;    (let [rotated-rotor (rotate-rotor rotor)]
;      (if rotated-rotor)))
;   )