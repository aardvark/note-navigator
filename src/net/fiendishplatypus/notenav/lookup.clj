(ns net.fiendishplatypus.notenav.lookup
  (:require [clojure.string]))


(def tuning-scale
  "Fret-by-fret guitar note scale.
  Distance between each note in collection is one fret"
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])


(defn upscale [{::keys [note octave]}]
  (loop [note   note
         octave octave
         scale  tuning-scale]
    (cond
      (and (= note (first scale))
           (not (nil? (second scale))))
      {::note (second scale) ::octave octave}

      (nil? (first scale)) {::note "C" ::octave (inc octave)}
      :else (recur note octave (drop 1 scale)))))


(defn calculate-string [starting-note frets]
  (loop [note starting-note
         i    0
         acc  {}]
    (if (> i frets)
      acc
      (recur (upscale note) (inc i) (conj acc {i note})))))


(def string->fret->note
  ""
  {6 (calculate-string {::note "E" ::octave 1} 12)
   5 (calculate-string {::note "A" ::octave 1} 12)
   4 (calculate-string {::note "D" ::octave 2} 12)
   3 (calculate-string {::note "G" ::octave 2} 12)
   2 (calculate-string {::note "B" ::octave 2} 12)
   1 (calculate-string {::note "E" ::octave 3} 12)})


(defn note [string fret]
  (-> string->fret->note
      (get string)
      (get fret)))


(defn pprint-note
  [{:app.lookup/keys [note octave]}]
  (str note octave))

(comment (pprint-note (note 2 5)))


(defn- pad-with-sym
  [pad-sym x]
  (str "|" pad-sym
       x
       (case (count (str x))
         0 (clojure.string/join (take 4 (repeat pad-sym)))
         1 (clojure.string/join (take 3 (repeat pad-sym)))
         2 (clojure.string/join (take 2 (repeat pad-sym)))
         3 (clojure.string/join (take 1 (repeat pad-sym))))))

(comment
  (pad-with-sym " " 1))


(defn pad-note [x]
  ((partial pad-with-sym "-") x))


(def fret-numbers
  (take 13 (iterate inc 0)))


(comment
  (clojure.string/join "\n"
                       [(str "  " (clojure.string/join (map (partial pad-with-sym " ") fret-numbers)))
                        (str 6 " " (apply str (map (comp pad-note pprint-note)
                                                   (vals (sort (get string->fret->note 6)))))
                             "|\n")]))


(comment
  "Print notes on fret up to 12"
  (clojure.string/join "\n"
                       [(str "  " (clojure.string/join (map (partial pad-with-sym " ") fret-numbers)) "|")
                        (clojure.string/join
                          "\n"
                          (map (fn [x]
                                 (let [[k v] x]
                                   (str k " " (apply str (map (comp pad-note pprint-note)
                                                              (vals (sort v))))
                                        "|")))
                               (reverse string->fret->note)))]))


(defn lookup-position [note octave]
  {::string 6 ::fret 0})

(calculate-string {::note "E" ::octave 1} 12)

(defn- find-by-note [note]
  (filter
    (fn [[_ v]] (= (::note v) note))))


(defn lookup-note
  [note]
 (into {} (map
            (fn [[string fret+notes]]
              [string (into {} (find-by-note note) fret+notes)])
            string->fret->note)))

(comment
  (lookup-note "A"))
