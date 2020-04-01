(ns net.fiendishplatypus.notenav.svg
  (:require [analemma.svg :as svg]
            [analemma.xml :as xml]
            [clojure.string]
            [net.fiendishplatypus.notenav.lookup :as lookup]))

(defn- vline
  [x y1 y2]
  (svg/line x y1 x y2 :stroke "black"))

(defn- hline
  [y x1 x2]
  (svg/line x1 y x2 y :stroke "black" :stroke-width "3"))

(comment
  "Draw finger placement diagram"
  (spit "test.svg"
        (let [x           60
              y           95
              string-line (fn [dx] (vline (+ x dx) (+ y 5) (+ y 155)))
              fret-line   (fn [dy] (hline (+ y dy) (+ x 30) (+ x 150)))]

          (xml/emit
            (svg/svg
              (xml/add-attrs (svg/text "6") :x (+ x 35) :y y)
              (string-line 40)

              (xml/add-attrs (svg/text "5") :x (+ x 55) :y y)
              (string-line (+ 40 20))

              (xml/add-attrs (svg/text "4") :x (+ x 75) :y y)
              (string-line (+ 40 20 20))

              (xml/add-attrs (svg/text "V") :x x :y (+ y 30))
              (fret-line 10)

              (xml/add-attrs (svg/text "VI") :x x :y (+ y 60))
              (fret-line (+ 10 30))

              (xml/add-attrs (svg/text "VII") :x x :y (+ y 90))
              (fret-line (+ 10 30 30))

              (xml/add-attrs (svg/text "VIII") :x x :y (+ y 120))
              (fret-line (+ 10 30 30 30))

              (xml/add-attrs (svg/text "IX") :x x :y (+ y 150))
              (fret-line (+ 10 30 30 30 30))

              (svg/circle (+ x 40 0) (+ y 25 0) 9 :fill "red" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "1") :x (+ x 36) :y (+ y 30) :stroke "white" :fill "white")

              (svg/circle (+ x 40 0) (+ y 25 30 30 30) 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "4") :x (+ x 36) :y (+ y 30 30 30 30) :stroke "black" :fill "black")

              (svg/circle (+ x 40 20) (+ y 25 0) 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "1") :x (+ x 36 20) :y (+ y 30) :stroke "black" :fill "black")

              (svg/circle (+ x 40 20) (+ y 25 30 30) 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "3") :x (+ x 36 20) :y (+ y 30 30 30) :stroke "black" :fill "black")

              (svg/circle (+ x 40 20 20) (+ y 25 0) 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "1") :x (+ x 36 20 20) :y 125 :stroke "black" :fill "black"))))))


(defn make-strings
  [start-x start-y]
  (reduce concat []
          (let [sn (atom 7)]
            (for [dx [40 60 80 100 120 140]]
              [(xml/add-attrs
                 (svg/text (str (swap! sn - 1)))
                 :x (+ start-x (- dx 5))
                 :y start-y)
               (vline (+ start-x dx) (+ start-y 5) (+ start-y 155))]))))

(comment
  (spit "test-strings.svg"
        (let [x 60
              y 95]
          (xml/emit
            (apply svg/svg
                   (make-strings x y))))))


(defn- make-fret
  [x y [dy fret-number]]
  [(hline (+ y 10 dy) (+ x 30) (+ x 150))
   (xml/add-attrs (svg/text fret-number) :x x :y (+ y 30 dy))])

;; [0 30 60 90]
(defn ladder
  [start step base]
  (loop [start  start
         step   step
         length (count base)
         res    []]
    (if (< (count res) length)
      (recur (+ start step) step length (cons start res))
      (into [] (reverse res)))))


(defn make-frets
  [x y frets]
  (reduce concat []
          (let [a   (ladder 0 40 frets)
                dys (into [] (map (fn [a b] [a b]) a frets))]
            (for [dy dys]
              (make-fret x y dy)))))

(comment
  (spit "test-frets.svg"
        (xml/emit
          (apply svg/svg
                 (make-frets 0 0 ["I" "II" "III" "IV"])))))

(comment
  (spit "test-fret-string.svg"
        (xml/emit
          (apply svg/svg
                 (concat
                   [{:width 200 :height 200}]
                   (make-frets 60 95 ["I" "II" "III" "IV"])
                   (make-strings 60 95))))))


;;circle dx is 20
;;       dy is 30
(defn finger-circle
  [x dx y dy text]
  (let [root? (clojure.string/includes? text "R")
        text  (clojure.string/replace text "R" "")
        style (if root?
                {:circle-fill "red"
                 :text-fill   "white"
                 :text-stroke "white"}
                {:circle-fill "white"
                 :text-fill   "black"
                 :text-stroke "black"})]
    [(svg/circle (+ x 40 dx) (+ y 25 dy) 9 :fill (style :circle-fill) :stroke "black" :stroke-width "1")
     (xml/add-attrs (svg/text text) :x (+ x 36 dx) :y (+ y 30 dy) :stroke (style :text-stroke) :fill (style :text-fill))]))


(comment
  (spit "test-circles.svg"
        (xml/emit
          (apply svg/svg
                 (concat []
                         [{:width 200 :height 200}]
                         (finger-circle 0 0 0 0 "1R")
                         (finger-circle 10 20 10 30 "5"))))))


(defn circle-row
  [x0 y0 row]
  (reduce concat []
          (let [joined-row (into [] (map (fn [a b] [a b]) (ladder 0 20 row) row))]
            (for [[dx finger-number] joined-row
                  :when ((complement empty?) finger-number)]
              (finger-circle x0 dx y0 0 finger-number)))))


(comment
  "Minor pentatonic scale variant 1"
  (spit "test-fret-string-circles.svg"
        (let [x0 10 y0 10]
          (xml/emit
            (apply svg/svg
                   (concat
                     [{:width 200 :height 200}]
                     (make-frets x0 y0 ["I" "II" "III" "IV" ""])
                     (make-strings x0 y0)
                     (circle-row x0 y0 ["1R" "1" "1" "1" "1" "1R"])
                     (circle-row x0 (+ y0 30) [])
                     (circle-row x0 (+ y0 30 30) ["" "3" "3R" "3" "" ""])
                     (circle-row x0 (+ y0 30 30 30) ["4" "" "" "" "4" "4"])))))))


(defn make-finger-circles
  [x0 y0 finger-placement]
  (reduce concat []
          (for [[dy row] (into [] (map (fn [a b] [a b])
                                       (ladder 14 40 finger-placement)
                                       finger-placement))]
            (circle-row x0 (+ y0 dy) row))))


(comment
  "Minor pentatonic scale variant 1"
  (spit "minor-pentatonic-1.svg"
        (let [x0 10 y0 10]
          (xml/emit
            (apply svg/svg
                   (concat
                     [{:width 200 :height 200}]
                     (make-frets x0 y0 ["I" "II" "III" "IV" ""])
                     (make-strings x0 y0)
                     (make-finger-circles x0 y0 [["1R" "1" "1" "1" "1" "1R"]
                                                 []
                                                 ["" "3" "3R" "3" "" ""]
                                                 ["4" "" "" "" "4" "4"]])))))))


(defn note
  [x0 dx y0 dy note]
  (let [effX (+ x0 dx)
        effY (+ y0 dy)]
    [(svg/rect effX (- effY 11) 13 22 :fill "white")
     (xml/add-attrs (svg/text note)
                    :x effX
                    :y effY
                    :stroke "black"
                    :font-size "0.6em"
                    :font-family "JetBrains Mono")]))


(defn note-row
  [x0 y0 row]
  (reduce concat []
          (let [joined-row (into [] (map (fn [a b] [a b]) (ladder 0 20 row) row))]
            (for [[dx tone] joined-row
                  :when ((complement empty?) tone)]
              (note x0 dx y0 0 tone)))))


(defn make-notes
  [x0 y0 notes]
  (reduce concat []
          (for [[dy row] (into [] (map (fn [a b] [a b])
                                       (ladder 14 40 notes)
                                       notes))]
            (note-row x0 (+ y0 dy) row))))

(spit "test-notes.svg"
      (xml/emit
        (apply svg/svg
               (concat [{:width 100 :height 100}]
                       (note 0 0 0 0 "E#3")))))

(conj [] (lookup/pprint-note (lookup/note 6 5))
      (lookup/pprint-note (lookup/note 5 5))
      (lookup/pprint-note (lookup/note 4 5))
      (lookup/pprint-note (lookup/note 3 5))
      (lookup/pprint-note (lookup/note 2 5))
      (lookup/pprint-note (lookup/note 1 5)))

(comment
  "Minor pentatonic scale variant 1 with notes"
  (spit "minor-pentatonic-with-notes.svg"
        (let [x0        10 y0 10
              input     {:1 ["1R" "1" "1" "1" "1" "1R"]
                         :2 []
                         :3 ["" "3" "3R" "3" "" ""]
                         :4 ["4" "" "" "" "4" "4"]}
              fret-dict {:1 "I" :2 "II" :3 "III" :4 "IV"}
              frets     (vals (select-keys fret-dict (keys input)))]
          (xml/emit
            (apply svg/svg
                   (concat
                     [{:width 200 :height 200}]
                     (make-frets x0 y0 (conj (vec frets) ""))
                     (make-strings x0 y0)
                     (make-finger-circles x0 y0 [["1R" "1" "1" "1" "1" "1R"]
                                                 []
                                                 ["" "3" "3R" "3" "" ""]
                                                 ["4" "" "" "" "4" "4"]])
                     (make-notes (+ x0 32) (+ y0 10) [["E#6" "E#5" "E#4" "E#3" "E#2" "E#1"] ["E3"]])))))))


(defn to-strings
  "Translate finger circles to the strings for future note assignment"
  [xs]
  (loop [xs    xs
         out   []
         start 6]
    (let [a (first xs)]
      (if (empty? xs)
        out
        (recur (rest xs)
               (conj out (if (empty? a) "" start))
               (- start 1))))))


(comment
  (map to-strings (vals {:1 ["1R" "1" "1" "1" "1" "1R"]
                         :2 []
                         :3 ["" "3" "3R" "3" "" ""]
                         :4 ["4" "" "" "" "4" "4"]}))
  "returns"
  [[6 5 4 3 2 1]
   []
   ["" 5 4 3 "" ""]
   [6 "" "" "" 2 1]])
