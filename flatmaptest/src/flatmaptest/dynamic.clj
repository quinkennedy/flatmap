(ns flatmaptest.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :geo (json/read (io/reader "/media/quin/data/docs/me/flatmap/export.geojson"))
   :xOffset -913000
   :yOffset -226200
   :xScale 10000
   :yScale 10000})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :geo (:geo state)
   :xOffset (/ -913000 4);(:xOffset state)
   :yOffset (/ -226100 4);(:yOffset state)
   :xScale 2500;(:xScale state)
   :yScale 2500});(:yScale state)})

(defn positionPoint [point]
  (let [divisor 9
		xScale (/ 10000 divisor)
		yScale (- 0 xScale)
        xOffset (/ -912000 divisor)
		yOffset (/ 230000 divisor)
        x (get point 0)
        x1 (* x xScale)
        x2 (+ x1 xOffset)
        y (get point 1)
        y1 (* y yScale)
        y2 (+ y1 yOffset)]
    [x2 y2]))

(defn adjustPoint [point]
  (let [rotation 0
        a [400 400]
        b [300 300]]))
        

(defn drawGeometry [state geometry]
  (let [points (get geometry "coordinates")]
    (if points
      (loop [i 0]
        (if (< i (count points))
          (let [point (get points i)
                ppoint (positionPoint point)
                x (get ppoint 0)
                y (get ppoint 1)]
            (q/point x y)
            (recur (inc i))))))))

(defn drawFeatures [state geo]
  (let [features (get geo "features")]
    (loop [i 0]
      (if (< i (count features))
         (let [feature (get features i)]
           (if (get feature "geometry")
             (drawGeometry state (get feature "geometry")))
           (recur (inc i)))))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (drawFeatures state (:geo state)))
