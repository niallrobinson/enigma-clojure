(ns enigma.core
  (:gen-class)
  (:use [debux core]))

(defn -main
  [& args]
  (println "Hello, World!"))

(def ref-map (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn char->int [c] (- (int c) 65)) ;to get a->1, b->2 etc

(defn map->deltas [rotor-map] (map #(- (char->int %1) (char->int %2)) rotor-map ref-map))

(def rotors {
  :I (with-meta (map->deltas (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ")) {:nudge 23 :rotation 0}),
  :II (with-meta (map->deltas (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE")) {:nudge 22 :rotation 0}),
  :III (with-meta (map->deltas (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO")) {:nudge 17 :rotation 0});,
    ; "IV"    : { mapper: "ESOVPZJAYQUIRHXLNFTGKDCMWB", step: "K"},
    ; "V"     : { mapper: "VZBRGITYUPSDNHLXAWMJQOFECK", step: "A"},
    ; "VI"    : { mapper: "JPGVOUMFYQBENHZRDKASXLICTW", step: "AN"},
    ; "VII"   : { mapper: "NZJHGRCXMYSWBOUFAIVLPEKQDT", step: "AN"},
    ; "VIII"  : { mapper: "FKQHTLXOCBJSPDZRAMEWNIUYGV", step: "AN"},
    ; "β"     : { mapper: "LEYJVCNIXWPBQMDRTAKZGFUHOS", step: ""},
    ; "γ"     : { mapper: "FSOKANUERHMBTIYCWLQPZXVGJD", step: ""}
    })

(def reflectors {
    :B (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT");,
    ; "C":    ['AF', 'BV', 'CP', 'DJ', 'EI', 'GO', 'HY', 'KR', 'LZ', 'MX', 'NW', 'TQ', 'SU'],
    ; "B Dünn":   ['AE', 'BN', 'CK', 'DQ', 'FU', 'GY', 'HW', 'IJ', 'LO', 'MP', 'RX', 'SZ', 'TV'],
    ; "C Dünn":   ['AR', 'BD', 'CO', 'EJ', 'FN', 'GT', 'HK', 'IV', 'LM', 'PW', 'QZ', 'SX', 'UY']
    })

(def plugboard {:A \B, :B \A, :C \D, :D \C, :E \F, :F \E,  :G \H, :H \G})

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
    (:rotation (meta rotor))
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

(defn get-delta
  [rotor letter]
  (nth rotor (mod
   (+ (:rotation (meta rotor)) (char->int letter))
   (count rotor))))

(defn rotor-encode
  [rotor letter]
  (nth ref-map
       (mod (+
              (char->int letter)
              (get-delta rotor letter))
            (count rotor))))

(defn rotors-encode
  ([rotors letter]
    (rotors-encode (first rotors) (rest rotors) letter))
  ([rotor rotors letter]
   (let [nextletter (rotor-encode rotor letter)]
     (dbg nextletter)
     (if (empty? rotors)
      nextletter
      (recur (first rotors) (rest rotors) nextletter))))
  )

(defn reflect [reflector letter] (dbg (nth reflector (char->int letter))) (nth reflector (char->int letter)))

(defn plug
  [plugboard letter]
  (let [p ((keyword (str letter)) plugboard)]
    (dbg (if p p letter))
    (if p p letter))
  )

(defn neg-seq [s] (with-meta (map #(* -1 %) s) (meta s)))
(defn flip-rotors [rotors] (into [] (reverse (map neg-seq rotors))))

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