(ns enigma.core
  (:gen-class)
  (:use [debux core]
        [clojure.set]))

(defn -main
  [& args]
  (println "Hello, World!"))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def raw-alphabet (seq alphabet))

(defn kv-map
  [ks vs]
  (zipmap (map keyword (map str ks)) vs))

(def rotors {
  :I {:letters-out (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ") :letters-in raw-alphabet :alphabet raw-alphabet :nudge \V},
  :II {:letters-out (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE") :letters-in raw-alphabet :alphabet raw-alphabet :nudge \E},
  :III {:letters-out (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO") :letters-in raw-alphabet :alphabet raw-alphabet :nudge \Q};,
    ; "IV"    : { mapper: "ESOVPZJAYQUIRHXLNFTGKDCMWB", step: "K"},
    ; "V"     : { mapper: "VZBRGITYUPSDNHLXAWMJQOFECK", step: "A"},
    ; "VI"    : { mapper: "JPGVOUMFYQBENHZRDKASXLICTW", step: "AN"},
    ; "VII"   : { mapper: "NZJHGRCXMYSWBOUFAIVLPEKQDT", step: "AN"},
    ; "VIII"  : { mapper: "FKQHTLXOCBJSPDZRAMEWNIUYGV", step: "AN"},
    ; "β"     : { mapper: "LEYJVCNIXWPBQMDRTAKZGFUHOS", step: ""},
    ; "γ"     : { mapper: "FSOKANUERHMBTIYCWLQPZXVGJD", step: ""}
    })

(def reflectors {
    :B (kv-map raw-alphabet (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT"));,
    ; "C":    ['AF', 'BV', 'CP', 'DJ', 'EI', 'GO', 'HY', 'KR', 'LZ', 'MX', 'NW', 'TQ', 'SU'],
    ; "B Dünn":   ['AE', 'BN', 'CK', 'DQ', 'FU', 'GY', 'HW', 'IJ', 'LO', 'MP', 'RX', 'SZ', 'TV'],
    ; "C Dünn":   ['AR', 'BD', 'CO', 'EJ', 'FN', 'GT', 'HK', 'IV', 'LM', 'PW', 'QZ', 'SX', 'UY']
    })

(def plugboard {:A \B, :B \A, :C \D, :D \C, :E \F, :F \E,  :G \H, :H \G})

(defn rotate
  [wheel]
    (concat (rest wheel) [(first wheel)])
  )

(defn flip-rotor
  [rotor]
  {:letters-in (:letters-out rotor), :letters-out (:letters-in rotor), :nudge (:nudge rotor)})

(defn rotate-rotor
  [rotor]
  (-> rotor 
      (update :letters-in rotate)
      (update :alphabet rotate)
  )
)

(defn nudge?
  [rotor]
  (= (first (:alphabet rotor))
     (:nudge rotor)
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

(defn codec
  [rotor letter]
  (dbg "codec")
  (dbg ((keyword (str letter)) (kv-map (:letters-in rotor) (:letters-out rotor))))
  ((keyword (str letter)) (kv-map (:letters-in rotor) (:letters-out rotor)))
)

(defn rotors-encode
  ([rotors letter]
    (rotors-encode (first rotors) (rest rotors) letter))
  ([rotor rotors letter]
   (let [nextletter (codec rotor letter)]
     (dbg nextletter)
     (if (empty? rotors)
      nextletter
      (recur (first rotors) (rest rotors) nextletter))))
  )

(defn reflect [reflector letter]
  (dbg "Reflector")
  (dbg (keyword (str letter) reflector))
  (keyword (str letter) reflector))

(defn plug
  [plugboard letter]
  (let [p ((keyword (str letter)) plugboard)]
    (dbg "Plugboard")
    (dbg (if p p letter))
    (if p p letter))
  )

(defn flip-rotors [rotors] (into [] (reverse (map flip-rotor rotors))))

(defn encode-letter 
  [rotors reflector plugboard letter]
  (->> letter
   (plug plugboard)
   (rotors-encode rotors)
   (reflect reflector)
   (rotors-encode (flip-rotors rotors))
   (plug plugboard))
  )

(defn encode-string
  ([rotors reflector plugboard string]
    (let [chars (seq (clojure.string/upper-case string))]
      (encode-string rotors reflector plugboard (first chars) (rest chars) []))
    )
  ([rotors reflector plugboard achar chars new-chars]
    (let [rotors (rotate-rotors rotors)
      new-chars (conj new-chars (encode-letter rotors reflector plugboard achar))]
      (if (empty? chars)
        (clojure.string/join new-chars)
        (recur rotors reflector plugboard (first chars) (rest chars) new-chars))
      )))

; (defn rotate
;   ([rotors]
;    (rotate (first rotors) (rest rotors)) [])
;   ([rotor rotors rotated-rotors]
;    (let [rotated-rotor (rotate-rotor rotor)]
;      (if rotated-rotor)))
;   )