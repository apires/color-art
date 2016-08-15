(ns color-art.core
  (:require [com.evocomputing.colors :refer :all]
            [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]
           (java.awt.image BufferedImage)))

(defn distinct-enough?
  [color1 color2]
  (let [[^float r1 ^float g1 ^float b1] (map rgb-int-to-unit-float color1)
        [^float r2 ^float g2 ^float b2] (map rgb-int-to-unit-float color2)]
    (if (or (> (Math/abs (- r1 r2) ) 0.25  )
            (> (Math/abs (- g1 g2) ) 0.25  )
            (> (Math/abs (- b1 b2) ) 0.25  ))
      (if (and (< (Math/abs (- r1 g1)) 0.03)
               (< (Math/abs (- r1 b1)) 0.03))
               (if (and (< (Math/abs (- r2 g2)) 0.03)
                        (< (Math/abs (- r2 b2)) 0.03))
                 false)
               true)
      false)))

(defn int->color
  [i]
  (create-color :a (bit-and 0xFF (unsigned-bit-shift-right i 24))
                :r (bit-and 0xFF (unsigned-bit-shift-right i 16))
                :g (bit-and 0xFF (unsigned-bit-shift-right i 8))
                :b (bit-and 0xFF i)))

(defn dark?
  [c]
  (let [[r g b _] (map rgb-int-to-unit-float (:rgba c))]
    (< (+ (* 0.2126 r)
          (* 0.7152 g)
          (* 0.0722 b)) 0.5)))

(defn black-or-white?
  [c]
  (let [[r g b _] (map rgb-int-to-unit-float (:rgba c))]
    (or (and (> r 0.91)
             (> g 0.91)
             (> b 0.91))
        (and (< r 0.09)
             (< g 0.09)
             (< b 0.09)))))

(defn min-saturation
  [color min-saturation]
  (let [[h s l] (:hsl color)]
    (if (< s min-saturation)
      (create-color :h h :s min-saturation :l l)
      color)))

(defn contrasts-with?
  [bg fg]
  (let [[r1 g1 b1] (map rgb-int-to-unit-float (:rgba bg))
        [r2 g2 b2] (map rgb-int-to-unit-float (:rgba fg))
        bg-lum (+ (* r1 0.2126) (* g1 0.7152) (* b1 0.0722) 0.05)
        fg-lum (+ (* r2 0.2126) (* g2 0.7152) (* b2 0.0722) 0.05)]
    (< 1.6 (if (> bg-lum fg-lum)
             (/ bg-lum fg-lum)
             (/ fg-lum bg-lum)))))

(defn color-frequencies
  [^ints pixels]
  (->> pixels
       (frequencies)
       (into (sorted-map))
       (sort-by val >)))

(defn extract-colors
  [image]
  (let [imageFile (->> image (io/resource) (io/file))
        ^BufferedImage image (ImageIO/read imageFile)
        ^ints border-pixels (.getRGB image 5 0 10 (.getHeight image) nil 0 10)
        ^ints pixels (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image))]
    {:border (color-frequencies border-pixels)
     :sampling (* 0.01 (.getHeight image))
     :image  (color-frequencies pixels)}))

(defn is-bg-color-better?
  [best-color this-color]
  (let [[_ best-count] best-color
        [this this-count] this-color
        ratio (/ this-count best-count)]
    (if (and (> ratio 0.3) (not (black-or-white? (first this))))
      this-color
      best-color)))

(defn pick-background
  [border-colors color-sample-size]
  (let [passes-sampling? #(> (second %1) color-sample-size)
        edge-colors (filter passes-sampling? border-colors)
        first-color (->> edge-colors (first) (first) (int->color)) ]
    (if-not (black-or-white? first-color) first-color
                                          (->> (reduce is-bg-color-better? edge-colors)
                                               (first)
                                               (int->color)))))

(defn pick-first-color
  [[color & colors] bg-color]
  (if (contrasts-with? bg-color color)
    [color colors]
    (pick-first-color colors bg-color)))

(defn pick-second-color
  [[color & colors] primary bg-color]
  (if (and (contrasts-with? bg-color color)
           (distinct-enough? (:rgba color) (:rgba primary)) )
    [color colors]
    (pick-second-color colors primary bg-color)))

(defn pick-detail-color
  [[color & colors] primary secondary bg-color]
  (if (and (contrasts-with? bg-color color)
           (distinct-enough? (:rgba color) (:rgba primary))
           (distinct-enough? (:rgba color) (:rgba secondary)))
    [color colors]
    (pick-detail-color colors primary secondary bg-color)))

(defn pick-foreground
  [colors background-color]
  (let [dark-bg (dark? background-color)
        matches-coloring? #(if (= (not dark-bg) (dark? (min-saturation %1 15))) (min-saturation %1 15) nil )
        valid-colors (->> colors (map first) (map int->color) (map matches-coloring?) (remove nil?))
        [primary rest] (pick-first-color valid-colors background-color)
        [secondary rest] (pick-second-color rest primary background-color)
        [detail _] (pick-detail-color rest primary secondary background-color)]
    [primary secondary detail]))

(defn foo
  "I don't do a whole lot."
  [x]
  (let [colors (extract-colors "Shawn-Lees-Ping-Pong-Orchestra.png")
        bg-color (pick-background (:border colors) (:sampling colors))
        fg-color (pick-foreground (:image colors) bg-color)]
    (println bg-color)
    (println fg-color)))