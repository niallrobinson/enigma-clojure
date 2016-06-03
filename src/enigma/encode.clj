(ns enigma.encode
  (:use [enigma.codecs]
        [enigma.rotorops]))

(defn encode-letter 
  [rotors reflector plugboard letter]
  (->> letter
   (plug plugboard)
   (rotors-encode rotors)
   (reflect reflector)
   (rotors-encode (flip-rotors rotors))
   (plug plugboard)))

(defn encode-string
  ([rotors reflector plugboard string]
    (let [chars (seq (clojure.string/upper-case string))]
      (encode-string rotors reflector plugboard (first chars) (rest chars) [])))
  ([rotors reflector plugboard achar chars new-chars]
    (let [rotors (rotate-rotors rotors)
      new-chars (conj new-chars (encode-letter rotors reflector plugboard achar))]
      (if (empty? chars)
        (clojure.string/join new-chars)
        (recur rotors reflector plugboard (first chars) (rest chars) new-chars)))))