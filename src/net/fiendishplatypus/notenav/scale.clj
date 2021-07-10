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
  (scale {::l/note "C" ::l/octave 3} minor))

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


(defn notes-to-positions
  [notes]
  (let [coll (mapcat (fn [{::l/keys [note octave]}]
                       (l/lookup-position note octave))
                     notes)
        frets (persistent!
               (reduce
                (fn [ret x]
                  (let [k (::l/fret x)]
                    (assoc! ret k (conj (get ret k [])
                                        (::l/string x)))))
                (transient {}) coll))]
    (into (sorted-map-by #(< (to-int %1) (to-int %2)))
          (map
           (fn [[k v]] 
             [(keyword (str k)) (number-to-strings v)])
           frets))))

(comment
  (notes-to-positions (scale {::l/note "C" ::l/octave 3} minor))

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

  (into
   (sorted-map-by #(< (to-int %1) (to-int %2)))
   (merge empty-fret
          (notes-to-positions (scale {::l/note "C" ::l/octave 3} minor))))

  (zipmap
   (range 1 9)
   (scale {::l/note "C" ::l/octave 3} minor))

  (mapcat (fn [{::l/keys [note octave]}]
            (l/lookup-position note octave))
          (scale {::l/note "C" ::l/octave 3} minor))

  (let [coll (mapcat (fn [[n {::l/keys [note octave]}]]
                  (map #(assoc % :octave-position n) (l/lookup-position note octave)))
                (zipmap
                 (range 1 9)
                 (scale {::l/note "C" ::l/octave 3} minor)))


        frets (persistent!
               (reduce
                (fn [acc x]
                  (let [fret (::l/fret x)
                        [strs octns] (get acc fret [[] []])
                        strs (conj strs (::l/string x))
                        octns (conj octns (:octave-position x))]
                    (assoc! acc fret [strs octns])))
                (transient {}) coll))]
    frets)
  
  )

(defn notes-to-positions-2
  [notes]
  (let [coll
        (mapcat (fn [[n {::l/keys [note octave]}]]
                  (map #(assoc % :octave-position n) (l/lookup-position note octave)))
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
          (map
           (fn [[k [v vv]]]
             [(keyword (str k)) (number-to-strings v vv)])
           frets))))

(notes-to-positions-2 (zipmap (range 1 9) (scale {::l/note "C" ::l/octave 3} major)))


