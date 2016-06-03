(ns enigma.core
  (:gen-class)
  (:use [debux core]
        [clojure.set]
        [enigma.constants]
        [enigma.utils]
        [enigma.rotorops]
        [enigma.codecs]
        [enigma.encode]
        [enigma.crack]))  

(defn output [args]
  (print args))

(defn get-output
  [args]
  (str "Enigma cracked with\n"
        "Rotors: " (map :name (nth args 0)) "\n"
        "Reflector:" (:name (meta (nth args 1))) "\n"
        "Plugboard: " (nth args 2) "\n"))

(defn -main
  [stringin stringout]
  (let [solution (crack
          (vals (select-keys rotors [:I :II :III]))
          3
          (vals (select-keys reflectors [:B :C]))
          [plugboard]
          stringin
          stringout)]
  (if solution
    (output solution)
    (println "No solution found"))))