(ns enigma.core
  (:gen-class))

(defn -main
  [& args]
  (println "Hello, World!"))

(def rotors {
    "I"     : { mapper: "EKMFLGDQVZNTOWYHXUSPAIBRCJ", step: "R"},
    "II"    : { mapper: "AJDKSIRUXBLHWTMCQGZNPYFVOE", step: "F"},
    "III"   : { mapper: "BDFHJLCPRTXVZNYEIWGAKMUSQO", step: "W"},
    "IV"    : { mapper: "ESOVPZJAYQUIRHXLNFTGKDCMWB", step: "K"},
    "V"     : { mapper: "VZBRGITYUPSDNHLXAWMJQOFECK", step: "A"},
    "VI"    : { mapper: "JPGVOUMFYQBENHZRDKASXLICTW", step: "AN"},
    "VII"   : { mapper: "NZJHGRCXMYSWBOUFAIVLPEKQDT", step: "AN"},
    "VIII"  : { mapper: "FKQHTLXOCBJSPDZRAMEWNIUYGV", step: "AN"},
    "β"     : { mapper: "LEYJVCNIXWPBQMDRTAKZGFUHOS", step: ""},
    "γ"     : { mapper: "FSOKANUERHMBTIYCWLQPZXVGJD", step: ""}
  })

(def reflectors {
    "B":    [:A "Y", :B "R", :C "U", :D "H", :E "Q", :F "S", :G "L", :I "P", :J "X", :K "N", :M "O", :T "Z", :V "W"];,
    ; "C":    ['AF', 'BV', 'CP', 'DJ', 'EI', 'GO', 'HY', 'KR', 'LZ', 'MX', 'NW', 'TQ', 'SU'],
    ; "B Dünn":   ['AE', 'BN', 'CK', 'DQ', 'FU', 'GY', 'HW', 'IJ', 'LO', 'MP', 'RX', 'SZ', 'TV'],
    ; "C Dünn":   ['AR', 'BD', 'CO', 'EJ', 'FN', 'GT', 'HK', 'IV', 'LM', 'PW', 'QZ', 'SX', 'UY']
  })

(defn rotate
  [rotor]
  (let [mapper (:mapper rotor)
        step (:step rotor)]
    {:mapper (str (apply str (rest a)) (first a))
     :step step}
    )
)

(defn char->int
  [c]
  (- (int c) 97) ;to get a->1, b->2 etc
  )

(defn arotorcode
  [in rotor]
  (nth rotor (char->int in))
  )

(defn rotorpass
  ([in rotors]
    (recur in (first rotors) (last rotors)))
  ([in rotor rotors]
    (if (empty? rotors))
   )
  )