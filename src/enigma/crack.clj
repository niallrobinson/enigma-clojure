(ns enigma.crack
  (:require [clojure.math.combinatorics :as combo]
            [swiss.arrows :refer :all])
  (:use [enigma.encode]
       [enigma.rotorops]))

(defn solution?
  [stringin stringout args]
  (= (apply encode-string (concat args [stringin]))
     stringout))

(defn get-rotor-rotations [rotor] (take 26 (iterate rotate-rotor rotor)))
(defn get-rotors-rotations [rotors] (apply combo/cartesian-product (map get-rotor-rotations rotors)))

(defn crack 
  [rotors nrotors reflectors plugboards stringin stringout]
  (let [rotor-combs (-<> rotors
                         (combo/combinations <> nrotors)
                         (map combo/permutations <>)
                         (apply concat <>)
                         (map get-rotors-rotations <>)
                         (apply concat <>))
        argss (combo/cartesian-product rotor-combs reflectors plugboards)]
        (first (filter (partial solution? stringin stringout) argss))))