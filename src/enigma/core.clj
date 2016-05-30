(ns enigma.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class)
  (:use [debux core]
        [clojure.set]))  

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def raw-alphabet (seq alphabet))

(defn kv-map [ks vs] (zipmap (map keyword (map str ks)) vs))
(defn char-get [c, m] ((keyword (str c)) m))

(def rotors {
  :I {:letters-out (seq "EKMFLGDQVZNTOWYHXUSPAIBRCJ") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\V} :name "I"},
  :II {:letters-out (seq "AJDKSIRUXBLHWTMCQGZNPYFVOE") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\E} :name "II"},
  :III {:letters-out (seq "BDFHJLCPRTXVZNYEIWGAKMUSQO") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\Q} :name "III"},
  :IV {:letters-out (seq "ESOVPZJAYQUIRHXLNFTGKDCMWB") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\K} :name "IV"},
  :V {:letters-out (seq "VZBRGITYUPSDNHLXAWMJQOFECK") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{\A} :name "V"},
  :β {:letters-out (seq "LEYJVCNIXWPBQMDRTAKZGFUHOS") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{} :name "β"},
  :γ {:letters-out (seq "FSOKANUERHMBTIYCWLQPZXVGJD") :letters-in raw-alphabet :alphabet raw-alphabet :nudge #{} :name "γ"},
  
    ; "VI"    : { mapper: "JPGVOUMFYQBENHZRDKASXLICTW", step: "AN"},
    ; "VII"   : { mapper: "NZJHGRCXMYSWBOUFAIVLPEKQDT", step: "AN"},
    ; "VIII"  : { mapper: "FKQHTLXOCBJSPDZRAMEWNIUYGV", step: "AN"},
  })

(def reflectors {
    :B (with-meta (kv-map raw-alphabet (seq "YRUHQSLDPXNGOKMIEBFZCWVJAT")) {:name "B"})
    :C (with-meta (kv-map raw-alphabet (seq "FVPJIAOYEDRZXWGCTKUQSBNMHL")) {:name "C"})
    :BDünn (with-meta (kv-map raw-alphabet (seq "ENKQAUYWJICOPBLMDXZVFTHRGS")) {:name "B Dünn"})
    :CDünn (with-meta (kv-map raw-alphabet (seq "RDOBJNTKVEHMLFCWZAXGYIPSUQ")) {:name "C Dünn"})
    })

(def plugboard {:A \B, :B \A, :C \D, :D \C, :E \F, :F \E,  :G \H, :H \G})

(defn rotate [wheel] (concat (rest wheel) [(first wheel)]))

(defn flip-rotor
  [rotor]
  {:letters-in (:letters-out rotor),
   :letters-out (:letters-in rotor),
   :alphabet (:alphabet rotor),
   :nudge (:nudge rotor)})

(defn rotate-rotor
  [rotor]
  (-> rotor 
      (update :letters-in rotate)
      (update :letters-out rotate)
  )
)

(defn nudge?
  [rotor]
  (contains? (:nudge rotor)
             (first (:letters-in rotor))
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
  (let [alphabet-out-map (kv-map (:alphabet rotor) (:letters-out rotor))
        in-alphabet-map (kv-map (:letters-in rotor) (:alphabet rotor))]
      (-> letter
          (char-get alphabet-out-map)
          (char-get in-alphabet-map))
  )
)

(defn rotors-encode
  ([rotors letter]
    (rotors-encode (first rotors) (rest rotors) letter))
  ([rotor rotors letter]
   (let [nextletter (codec rotor letter)]
     (if (empty? rotors)
      nextletter
      (recur (first rotors) (rest rotors) nextletter))))
  )

(defn reflect [reflector letter] (char-get letter reflector))

(defn plug
  [plugboard letter]
  (let [p ((keyword (str letter)) plugboard)]
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

(defn solution?
  [stringin stringout args]
  (= (apply encode-string (concat args [stringin]))
     stringout)
)

(defn output
  [args]
  (print "Enigma cracked with\n"
         "Rotors: " (map :name (nth args 0)) "\n"
         "Reflector:" (:name (meta (nth args 1))) "\n"
         "Plugboard: " (nth args 2) "\n")
)

(defn crack 
  [rotors nrotors reflectors plugboards stringin stringout]
  (let [rotor-combs (apply concat (map combo/permutations (combo/combinations rotors nrotors)))
        argss (combo/cartesian-product rotor-combs reflectors plugboards)]
        (println "Calculating" (count argss) "different solutions...\n") (flush)
        (first (filter (partial solution? stringin stringout) argss))
  )
)

(defn -main
  [stringin stringout]
  (let [solution (crack
          (vals (select-keys rotors [:I :II :III :IV :V :β :γ]))
          3
          (vals (select-keys reflectors [:B :C :BDünn :CDünn]))
          [plugboard]
          stringin
          stringout)]
  (if solution
    (output solution)
    (println "No solution found"))
  )
)