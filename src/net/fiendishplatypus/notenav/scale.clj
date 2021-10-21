(ns net.fiendishplatypus.notenav.scale
  (:require [net.fiendishplatypus.notenav.lookup :as l]
            [clojure.string :as s]))

(def major
  "Major scale in semitones"
  [2 2 1 2 2 2 1 1])

(def minor
  "Minor scale in semitones"
  [2 1 2 2 1 2 2 1])

(defn scale
  "Given a root note and a scale type
   produce list of notes in a scale"
  [root-note scale]
  (loop [scale scale
         note root-note
         acc '()]
    (if (empty? scale)
      (reverse acc)
      (recur
       (rest scale)
       (l/upscale note (first scale))
       (conj acc note)))))

(comment
  (scale {::l/note "C" ::l/octave 3} major))

(defn number-to-strings
  ([input]
   (let [base {6 "" 5 "" 4 "" 3 "" 2 "" 1 ""}]
     (loop [acc base
            [x & xs] input]
       (if (nil? x)
         (vals acc)
         (recur (assoc acc x 0) xs)))))
  ([input idxs]
   (let [base {6 "" 5 "" 4 "" 3 "" 2 "" 1 ""}]
     (loop [acc base
            [x & xs] input
            [n & nn] idxs]
       (if (nil? x)
         (vals acc)
         (recur (assoc acc x n) xs nn))))))

(comment
  (number-to-strings [3] 22)
  ;; => ("" "" "" " " "" "")
  (number-to-strings [5 2] [3 4])
  ;; => ("" " " "" "" " " "")
  (number-to-strings [2 4]))
  ;; => ("" "" " " "" " " "")


(defn to-int
  [x]
  (Integer/parseInt (name x)))


(def empty-fret
  {:1 '("" "" "" "" "" "")
   :2 '("" "" "" "" "" "")
   :3 '("" "" "" "" "" "")
   :4 '("" "" "" "" "" "")
   :5 '("" "" "" "" "" "")
   :6 '("" "" "" "" "" "")
   :7 '("" "" "" "" "" "")
   :8 '("" "" "" "" "" "")
   :9 '("" "" "" "" "" "")
   :10 '("" "" "" "" "" "")
   :11 '("" "" "" "" "" "")
   :12 '("" "" "" "" "" "")})


(defn notes-to-positions
  [notes]
  (let [coll (mapcat (fn [[n {::l/keys [note octave]}]]
                       (map #(assoc % :octave-position (str n))
                            (l/lookup-position note octave)))
                notes)
        frets (persistent!
               (reduce
                (fn [acc x]
                  (let [fret (::l/fret x)
                        [strs octns] (get acc fret [[] []])
                        strs (conj strs (::l/string x))
                        octns (conj octns (:octave-position x))]
                    (assoc! acc fret [strs octns])))
                (transient {}) coll))]

    (into (sorted-map-by #(< (to-int %1) (to-int %2)))
          (map (fn [[k [v vv]]]
                 [(keyword (str k)) (number-to-strings v vv)])
               frets))))


(defn to-diagram
  [tonic scale-type]
  (apply merge-with
         (fn [val-in-res val-in-later]
           (map (fn [a b] (if (s/blank? b) a b))
                val-in-res val-in-later))
         (cons
          empty-fret
          (for [o (list 0 1 2 3 4 5)]
            (notes-to-positions (zipmap (list "R" "2" "3" "4" "5" "6" "7" "R")
                                        (scale {::l/note tonic ::l/octave o} scale-type)))))))

