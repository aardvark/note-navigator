(ns net.fiendishplatypus.notenav.lookup-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.fiendishplatypus.notenav.lookup :refer [note lookup-position pad-note]]))

(deftest note-test
  (testing "note should return correct note and octave"
    (is (= #:app.lookup{:note "E" :octave 1} (note 6 0)))
    (is (= #:app.lookup{:note "E" :octave 2} (note 6 12)))
    (is (= #:app.lookup{:note "E" :octave 2} (note 5 7)))
    (is (= #:app.lookup{:note "E" :octave 2} (note 4 2))))


  (testing "lookup-position should return correct position given note and octave"
    (is (= #:app.lookup{:string 6 :fret 0} (lookup-position "E" 1)))))

(deftest pad-note-test
  (testing "`pad-note` pads note with '-' up to 4 size"
    (is (= (pad-note "") "|-----"))
    (is (= (pad-note "E") "|-E---"))
    (is (= (pad-note "E1") "|-E1--"))
    (is (= (pad-note "F#1") "|-F#1-"))))