(ns enigma.core-test
  (:require [clojure.test :refer :all]
            [enigma.core :refer :all])
  (:use [debux core]))

(deftest test-codec
  (is (=
      (codec (:I rotors) \A)
      \E))
  )

(deftest test-rotate
  (is (=
      (codec (rotate-rotor (:I rotors)) \A)
      \J))
  )

(deftest test-plugboard
  (is (= 
        (plug plugboard \A)
        \B))
  (is (= 
        (plug plugboard \Z)
        \Z))
)

(deftest test-flip-rotor
  (is (= 
        (let [r (:I rotors)]
          (->> \A
              (codec r)
              (codec (flip-rotor r))
          )
        )
        \A
)))

(deftest test-one-rotors-pass
  (is (=
        (rotors-encode [(:III rotors)
                        (:II rotors)
                        (:I rotors)] \A)
        \Z))
(is (=
      (rotors-encode (rotate-rotors [(:III rotors)
                                     (:II rotors)
                                     (:I rotors)]) \G)
      \O))
)

(deftest test-encode-letter
  (testing "Encoding a letter")
  (is (=
        (encode-letter
          (rotate-rotors [(:III rotors) (:II rotors) (:I rotors)])
          (:B reflectors)
          plugboard
          \H)
        \X
        ))
)


(deftest test-solution?
  (let [args  [[(:III rotors) (:II rotors) (:I rotors)]
              (:B reflectors)
              plugboard ]
       ]
    (is (solution? "HELLOWORLD" "XKACBBMTBF" args))
  )
)

(deftest test-encode-string
  (testing "Encoding then decoding gives the same thing")
  (let [encoded (encode-string
                  [(:III rotors) (:II rotors) (:I rotors)]
                   (:B reflectors)
                   plugboard
                   "HELLOWORLD")]
  (is (= (encode-string
                  [(:III rotors) (:II rotors) (:I rotors)]
                   (:B reflectors)
                   plugboard
                   encoded)
        "HELLOWORLD"
        ))))

(deftest test-crack
  (is (=
        (crack
          (vals (select-keys rotors [:I :II :III]))
          3
          (vals (select-keys reflectors [:B]))
          [plugboard]
          "XKHCELGHAX"
          "HEILHITLER")
        [(vals (select-keys rotors [:III :II :I]))
          (:B reflectors)
          {:A \B, :B \A, :C \D, :D \C, :E \F, :F \E,  :G \H, :H \G}]
)))