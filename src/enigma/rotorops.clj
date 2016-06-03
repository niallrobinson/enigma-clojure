(ns enigma.rotorops)

(defn nudge?
  [rotor]
  (contains? (:nudge rotor)
             (first (:letters-in rotor))))

(defn flip-rotor
  [rotor]
  {:letters-in (:letters-out rotor),
   :letters-out (:letters-in rotor),
   :alphabet (:alphabet rotor),
   :nudge (:nudge rotor)})

(defn flip-rotors [rotors] (into [] (reverse (map flip-rotor rotors))))

(defn rotate [wheel] (concat (rest wheel) [(first wheel)]))

(defn rotate-rotor
  [rotor]
  (-> rotor 
      (update :letters-in rotate)
      (update :letters-out rotate)))

(defn rotate-rotors
  ([rotors]
    (rotate-rotors (first rotors) (rest rotors) []))
  ([rotor rotors rotated-rotors]
    (let [updated-rotors (conj rotated-rotors (rotate-rotor rotor))]
      (if (and (seq rotors) (nudge? rotor))
        (rotate-rotors (first rotors) (rest rotors) updated-rotors)
        (into updated-rotors rotors)))))

