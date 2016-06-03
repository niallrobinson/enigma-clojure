(ns enigma.codecs
  (:use [enigma.utils]))

(defn codec
  [rotor letter]
  (let [alphabet-out-map (kv-map (:alphabet rotor) (:letters-out rotor))
        in-alphabet-map (kv-map (:letters-in rotor) (:alphabet rotor))]
      (-> letter
          (char-get alphabet-out-map)
          (char-get in-alphabet-map))))

(defn rotors-encode
  ([rotors letter]
    (rotors-encode (first rotors) (rest rotors) letter))
  ([rotor rotors letter]
   (let [nextletter (codec rotor letter)]
     (if (empty? rotors)
      nextletter
      (recur (first rotors) (rest rotors) nextletter)))))

(defn reflect [reflector letter] (char-get letter reflector))

(defn plug
  [plugboard letter]
  (let [p ((keyword (str letter)) plugboard)]
    (if p p letter)))