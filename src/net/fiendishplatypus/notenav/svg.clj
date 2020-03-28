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


(defn- make-fret [x y [dy fret-number]]
  [(hline (+ y 10 dy) (+ x 30) (+ x 150))
   (xml/add-attrs (svg/text fret-number) :x x :y (+ y 30 dy))])

(defn make-frets [x y frets]
  (reduce concat []
          (let [a   [0 30 60 90]
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


(defn finger-circle [x dx y dy & root?]
  (let [style (if root?
                {:circle-fill "red"
                 :text-fill   "white"
                 :text-stroke "white"}
                {:circle-fill "white"
                 :text-fill   "black"
                 :text-stroke "black"})]
    [(svg/circle (+ x 40 dx) (+ y 25 dy) 9 :fill (style :circle-fill) :stroke "black" :stroke-width "1")
     (xml/add-attrs (svg/text "1") :x (+ x 36 dx) :y (+ y 30 dy) :stroke (style :text-stroke) :fill (style :text-fill))]))

(comment
  (spit "test-circles.svg"
        (xml/emit
          (apply svg/svg
                 (concat []
                         [{:width 200 :height 200}]
                         (finger-circle 0 0 0 0 true)
                         (finger-circle 10 20 10 30))))))
