(ns net.fiendishplatypus.notenav.scale
  (:require [net.fiendishplatypus.notenav.lookup :as l]))

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
  (scale {::l/note "D" ::l/octave 4} minor))

