(ns net.fiendishplatypus.notenav.lookup-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.fiendishplatypus.notenav.lookup :as lookup]))


(deftest note-test
  (testing "`lookup-note` should return correct note and octave"
    (is (= (lookup/note 6 0) {::lookup/note "E" ::lookup/octave 1}))
    (is (= (lookup/note 6 12) {::lookup/note "E" ::lookup/octave 2}))
    (is (= (lookup/note 5 7) {::lookup/note "E" ::lookup/octave 2}))
    (is (= (lookup/note 4 2) {::lookup/note "E" ::lookup/octave 2})))

  (testing "`lookup-position` should return correct position given note and octave"
    (is (= (lookup/lookup-position "E" 1) 
           '({::lookup/string 6 ::lookup/fret 0})))
    (is (= (lookup/lookup-position "A" 1)
           '({::lookup/string 6 ::lookup/fret 5}
             {::lookup/string 5 ::lookup/fret 0})))))


(deftest pad-note-test
  (testing "`pad-note` pads note with '-' up to 4 size"
    (is (= (lookup/pad-note "") "|-----"))
    (is (= (lookup/pad-note "E") "|-E---"))
    (is (= (lookup/pad-note "E1") "|-E1--"))
    (is (= (lookup/pad-note "F#1") "|-F#1-"))))

(deftest upscale-test
  (testing "`upscale` increases passed tone by semitone."
    (testing "Tone shift. Given: C1 should return C#1."
      (is (= {::lookup/note "C#" ::lookup/octave 1}
             (lookup/upscale {::lookup/note "C" ::lookup/octave 1}))))
    (testing "Octave shift. Given: B1 should return C2. "
      (is (= {::lookup/note "C" ::lookup/octave 2}
             (lookup/upscale {::lookup/note "B" ::lookup/octave 1}))))
    (testing "Tone shift. Given tone shifts will shift by number of semitones"
      (is (= {::lookup/note "D" ::lookup/octave 1}
             (lookup/upscale {::lookup/note "C" ::lookup/octave 1} 2))))))

(defn run-tests 
  []
  (clojure.test/run-tests (symbol (str (.getName *ns*)))))

(comment
  (run-tests))
