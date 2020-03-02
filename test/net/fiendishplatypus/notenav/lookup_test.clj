(ns net.fiendishplatypus.notenav.lookup-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.fiendishplatypus.notenav.lookup :as lookup]))


(deftest note-test
  (testing "note should return correct note and octave"
    (is (= {::lookup/note "E" ::lookup/octave 1} (lookup/note 6 0)))
    (is (= {::lookup/note "E" ::lookup/octave 2} (lookup/note 6 12)))
    (is (= {::lookup/note "E" ::lookup/octave 2} (lookup/note 5 7)))
    (is (= {::lookup/note "E" ::lookup/octave 2} (lookup/note 4 2))))


  (testing "lookup-position should return correct position given note and octave"
    (is (= {::lookup/string 6 ::lookup/fret 0} (lookup/lookup-position "E" 1)))))


(deftest pad-note-test
  (testing "`pad-note` pads note with '-' up to 4 size"
    (is (= (lookup/pad-note "") "|-----"))
    (is (= (lookup/pad-note "E") "|-E---"))
    (is (= (lookup/pad-note "E1") "|-E1--"))
    (is (= (lookup/pad-note "F#1") "|-F#1-"))))