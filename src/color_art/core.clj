(ns color-art.core
  (:require [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]
           (java.awt.image BufferedImage)
           (java.awt Color)))

;(defn distinct-enough?
;  [color1 color2]
;  (let [[^float r1 ^float g1 ^float b1] (map rgb-int-to-unit-float color1)
;        [^float r2 ^float g2 ^float b2] (map rgb-int-to-unit-float color2)]
;    (if (or (> (Math/abs (- r1 r2) ) 0.25  )
;            (> (Math/abs (- g1 g2) ) 0.25  )
;            (> (Math/abs (- b1 b2) ) 0.25  ))
;      (if (and (< (Math/abs (- r1 g1)) 0.03)
;               (< (Math/abs (- r1 b1)) 0.03))
;               (if (and (< (Math/abs (- r2 g2)) 0.03)
;                        (< (Math/abs (- r2 b2)) 0.03))
;                 false)
;               true)
;      false)))
;
;(defn int->color
;  [color-int]
;  (let [color (new Color color-int)
;       [r g b] (.getRGBComponents color nil)]
;    (create-color :r (int (* 255 r)) :g (int (* 255 g)) :b (int (* 255 b)))))
;
;(defn dark?
;  [c]
;  (let [[r g b _] (map rgb-int-to-unit-float (:rgba c))]
;    (< (+ (* 0.2126 r)
;          (* 0.7152 g)
;          (* 0.0722 b)) 0.5)))

(defn black-or-white?
  [c]
  (let [color (new Color c)
        ^floats [r g b _] (.getRGBComponents color nil)]
    (or (and (> r 0.91)
             (> g 0.91)
             (> b 0.91))
        (and (< r 0.09)
             (< g 0.09)
             (< b 0.09)))))

;(defn min-saturation
;  [color]
;  (let [[r g b] (:rgba color)
;        [^float h ^float s ^float b] (Color/RGBtoHSB r g b nil)]
;    (if (< s 0.15)
;      (int->color (Color/HSBtoRGB h 0.15 b))
;      color)))
;
;(defn contrasts-with?
;  [bg fg]
;  (let [[r1 g1 b1] (map rgb-int-to-unit-float (:rgba bg))
;        [r2 g2 b2] (map rgb-int-to-unit-float (:rgba fg))
;        bg-lum (+ (* r1 0.2126) (* g1 0.7152) (* b1 0.0722) 0.05)
;        fg-lum (+ (* r2 0.2126) (* g2 0.7152) (* b2 0.0722) 0.05)]
;    (< 1.6 (if (> bg-lum fg-lum)
;             (/ bg-lum fg-lum)
;             (/ fg-lum bg-lum)))))

(defn color-frequencies
  [^ints pixels]
  (->> pixels
       (frequencies)
       (sort-by val <)))

(defn extract-colors
  [image]
  (let [imageFile (->> image (io/resource) (io/file))
        ^BufferedImage oImage (ImageIO/read imageFile)
        ^BufferedImage image (new BufferedImage (.getWidth oImage) (.getHeight oImage) BufferedImage/TYPE_INT_RGB)
        _ (-> image (.getGraphics) (.drawImage oImage 0 0 nil))
        ;^ints border-pixels (.getRGB border 0 0 (.getWidth border) (.getHeight border) nil 0 (.getWidth border))
        ^ints pixels (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image))
        ^ints border-pixels (for [x (range 5 11)
                                  y (range 0 (.getHeight image))]
                              (get pixels (+ x (* (.getWidth image) y)))) ]

    (println "h" (.getHeight image) "w" (.getWidth image))
    (println "bp" (count border-pixels))

    {:border (color-frequencies border-pixels)
     :sampling (* 0.01 (.getHeight image))
     :image  (color-frequencies pixels)}))

(defn background-improver
  [a b]
  (let [[_ best-weight] a
        [candidate candidate-weight] b
        ratio (/ candidate-weight best-weight)]
    (if (and (> ratio 0.3)
             (not (black-or-white? candidate)))
      b
      a)))

(defn pick-background
  [border-colors color-sample-size]
  (let [passes-sampling? #(> (second %1) color-sample-size)
        edge-colors (->> border-colors (filter passes-sampling?) )]
    (cond
        (empty? edge-colors) 0
        :else (reduce background-improver edge-colors))))
;
;(defn pick-first-color
;  [[color & colors] bg-color]
;  (if (nil? color) (if (dark? bg-color) (create-color :r 255 :g 255 :b 255) (create-color :r 0 :g 0 :b 0))
;                   (if (contrasts-with? bg-color color)
;                     [color colors]
;                     (recur colors bg-color))))
;
;(defn pick-second-color
;  [[color & colors] primary bg-color]
;  (if (nil? color) (if (dark? bg-color) (create-color :r 255 :g 255 :b 255) (create-color :r 0 :g 0 :b 0))
;                   (if (and (contrasts-with? bg-color color)
;                            (distinct-enough? (:rgba color) (:rgba primary)))
;                     [color colors]
;                     (recur colors primary bg-color))))
;
;(defn pick-detail-color
;  [[color & colors] primary secondary bg-color]
;  (if (nil? color)
;    (if (dark? bg-color) [(create-color :r 255 :g 255 :b 255) []] [(create-color :r 0 :g 0 :b 0) []])
;    (if (and (contrasts-with? bg-color color)
;             (distinct-enough? (:rgba color) (:rgba primary))
;             (distinct-enough? (:rgba color) (:rgba secondary)))
;      [color colors]
;      (recur colors primary secondary bg-color))))
;
;(defn pick-foreground
;  [colors background-color]
;  (let [dark-bg (dark? background-color)
;        matches-coloring? #(if (= (not dark-bg) (dark? (min-saturation %1))) (min-saturation %1) nil )
;        valid-colors (->> colors (map first) (map int->color) (map matches-coloring?) (remove nil?))
;        [primary rest] (pick-first-color valid-colors background-color)
;        [secondary rest] (pick-second-color rest primary background-color)
;        [detail _] (pick-detail-color rest primary secondary background-color)]
;    [primary secondary detail]))

(defn foo
  "I don't do a whole lot."
  [x]
  (let [colors (extract-colors "max-thumbnail-hungry-af.jpg")
        bg-color (pick-background (:border colors) (:sampling colors))
        ;fg-color (pick-foreground (:image colors) bg-color)
        ]
    (println (new Color (first bg-color)))
    ;(println (map rgb-hexstr fg-color))
    ))

; Primary #583026
; Accent #EBE8E3