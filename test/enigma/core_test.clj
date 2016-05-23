(ns enigma.core-test
  (:require [clojure.test :refer :all]
            [enigma.core :refer :all])
  (:use [debux core]))

(deftest test-char->int
  (testing "Char as int in the alphabet")
  (is (= (char->int \A) 0))
  (is (= (char->int \Z) 25))
  )

(deftest test-rotate-rotor
  (testing "Update the rotor key with the rotation")
  (is (= 
        (:rotation (meta (rotate-rotor (:I rotors))))
        1
    ))
  (is (= 
        (:rotation (meta (rotate-rotor (:I rotors) 28)))
        2
    ))
  )

(deftest test-rotor-encode
  (testing "Make sure a rotor encodes a char to the correct letter")
  (is (=
        (rotor-encode (:I rotors) \B)
        \K
        )))

(deftest test-rotors-encode
  (testing "Right letter with multiple rotors")
  (is (=
        (rotors-encode [(:I rotors) (:II rotors) (:III rotors)] \A)
        \G
        )))

(deftest test-rotate-rotors
  (testing "Sequence of rotors and nudges")
  (is (= (let [rotated-rotors (rotate-rotors [(:I rotors) (:II rotors) (:III rotors)])]
           (map #(:rotation (meta %)) rotated-rotors)
         )
       [1 0 0]
      )
  )
)