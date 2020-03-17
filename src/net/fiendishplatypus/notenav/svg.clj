(ns net.fiendishplatypus.notenav.svg
  (:require [analemma.svg :as svg]
            [analemma.xml :as xml]))

(defn- vline [x y1 y2]
  (svg/line x y1 x y2 :stroke "black"))

(defn- hline [y x1 x2]
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
              (string-line 60)

              (xml/add-attrs (svg/text "4") :x (+ x 75) :y y)
              (string-line 80)

              (xml/add-attrs (svg/text "V") :x x :y (+ y 30))
              (fret-line 10)

              (xml/add-attrs (svg/text "VI") :x x :y (+ y 60))
              (fret-line 40)

              (xml/add-attrs (svg/text "VII") :x x :y (+ y 90))
              (fret-line 70)

              (xml/add-attrs (svg/text "VIII") :x x :y (+ y 120))
              (fret-line 100)

              (xml/add-attrs (svg/text "IX") :x x :y (+ y 150))
              (fret-line 130)

              (svg/circle 100 120 9 :fill "red" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "1") :x (+ x 36) :y 125 :stroke "white" :fill "white")

              (svg/circle 120 120 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "1") :x (+ x 56) :y 125 :stroke "black" :fill "black")

              (svg/circle 100 210 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "4") :x (+ x 36) :y 215 :stroke "black" :fill "black")

              (svg/circle 120 180 9 :fill "white" :stroke "black" :stroke-width "1")
              (xml/add-attrs (svg/text "3") :x (+ x 56) :y 185 :stroke "black" :fill "black"))))))


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


(defn- make-fret [x y [dy fret-number]]
  [(hline (+ y 10 dy) (+ x 30) (+ x 150))
   (xml/add-attrs (svg/text fret-number) :x x :y (+ y 30 dy))])

(defn make-frets [frets]
  (reduce concat []
          (let [x 60 y 95
                a [0 30 60 90]
                dys (into [] (map (fn [a b] [a b]) a frets))]
            (for [dy dys]
              (make-fret x y dy)))))

(comment
  (spit "test-frets.svg"
        (xml/emit
          (apply svg/svg
                 (make-frets ["I" "II" "III" "IV"])))))


