(ns flatmaptest.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 1)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :geo (json/read (io/reader "/media/quin/data/docs/me/flatmap/export.geojson"))
   :xOffset (/ -913000 4)
   :yOffset (/ -226100 4)
   :xScale (/ 10000 4)
   :yScale (/ 10000 4)})

(defn PtoC [r a]
  [(* (Math/cos a) r) (* (Math/sin a) r)])

(defn CtoP [x y]
  [(getDistance [0 0] [x y])
   (getAngle [0 0] [x y])])

(defn mouse-click [state event]
  (q/debug "clicked")
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :geo (:geo state)
   :xOffset (- (:xOffset state) 100)
   :yOffset (:yOffset state)
   :xScale (:xScale state)
   :yScale (:yScale state)})

; returns the angle from horizontal right,
; so getAngle [[0 0] [-1 0]] -> PI
; getAngle [[0 0] [0 -1] -> PI*3/2
(defn getAngle [pointA pointB]
  (let [xDiff (- (first pointB) (first pointA))
        yDiff (- (second pointB) (second pointA))
		angle (Math/atan2 yDiff xDiff)]
    (if (< angle 0)
      (+ (* 2 Math/PI) angle)
      angle)))

(defn getDistance [pointA pointB]
  (let [xDiff (- (first pointB) (first pointA))
        yDiff (- (second pointB) (second pointA))]
    (Math/sqrt (+ (* xDiff xDiff) (* yDiff yDiff)))))

(defn remap [x fromMin fromMax toMin toMax]
  (+ (* (/ (- x fromMin) (- fromMax fromMin)) (- toMax toMin)) toMin))

(defn adjustPoint [point]
  (let [rotation 0
        refX 500 
        refY 250
        changeAngle (- Math/PI rotation)
        pointAngle (getAngle [refX refY] point)
        newAngle (if (< pointAngle changeAngle)
                   (remap 
                     pointAngle 
                     0 changeAngle 
                     0 Math/PI)
                   (remap 
                     pointAngle 
                     changeAngle (* 2 Math/PI) 
                     Math/PI (* 2 Math/PI)))
        pointDist (getDistance [refX refY] point)
        relativePoint (PtoC pointDist newAngle)]
    [(+ refX (first relativePoint)) (+ refY (second relativePoint))]))

(defn getColor [origPoint newPoint]
  [(Math/sqrt 
    (+ 
      (Math/pow (- (first origPoint) (first newPoint)) 2)
      (Math/pow (- (second origPoint) (second newPoint)) 2))) 
   255 255])

(defn drawCircles []
  (let [rStep 4
		rMax 300
        aCount 30]
    (loop [r rStep]
      (if (< r rMax)
        (let []
          (loop [a 0]
            (if (< a aCount)
              (let [rad (/ (* (* 2 Math/PI) a) aCount)
                    relativePoint (PtoC r rad)
                    absPoint [(+ (/ (q/width) 2) (first relativePoint))
                              (+ (/ (q/height) 2) (second relativePoint))]
                    newPoint (adjustPoint absPoint)
                    color (getColor absPoint newPoint)]
                (apply q/stroke color)
                (q/point (first newPoint) (second newPoint))
                (recur (inc a)))))
          (recur (+ r rStep)))))))


(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :geo (:geo state)
   :xOffset (:xOffset state)
   :yOffset (:yOffset state)
   :xScale (:xScale state)
   :yScale (:yScale state)})

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

(def path [[91.63 22.57] [91.58 22.615] [91.5 22.7] [91.45 22.73] [91.4 22.74] [91.35 22.74]])

(def preStart [(+ 1 (first (first path))) (second (first path))])

(defn moveFromTo [point from to]
  [(+ (- (get point 0) (get from 0)) (get to 0))
   (+ (- (get point 1) (get from 1)) (get to 1))])

(defn rotate [point reference angle]
  (let [currAngle (getAngle reference point)
        newAngle (mod (+ currAngle angle) (* Math/PI 2))
        distance (getDistance reference point)
        newPoint (PtoC distance newAngle)]
    ;(q/debug (str point "," newPoint))
    ;(q/debug (str currAngle "," newAngle))
    (moveFromTo (PtoC distance newAngle) [0 0] [(q/width) (/ (q/height) 2)])))

(defn straightenFullPath [point path]
  (let [  
  ;first rotate point and 'rest' of path 
  ;based on first point in path and angle to next point
        positionedPoint (moveFromTo point 
                                    (first path)
                                    [(q/width) (/ (q/height) 2)])
        firstAngleDiff (- Math/PI (getAngle (first path) 
                                 (second path)))
        rotatedPoint (rotate positionedPoint
                             [(q/width) (/ (q/height) 2)]
                             firstAngleDiff)]
    rotatedPoint)
  ;for each point in path
  ; find distance to previous point
  ; use that to place point to the left of 
  ; previously-straightened point
)

(defn drawPoint [point]
  (let [ppoint (positionPoint point)
        apoint (adjustPoint ppoint)
		npoint (straightenFullPath apoint (map adjustPoint (map positionPoint path)))
        x (get npoint 0)
        y (get npoint 1)]
    (q/point x y)))
    
(defn drawGeometry [state geometry]
  (let [points (get geometry "coordinates")]
    (if points
      (loop [i 0]
        (if (< i (count points))
          (let [point (get points i)]
            (drawPoint point)
            (recur (inc i))))))))

(defn drawFeatures [state geo]
  (let [features (get geo "features")]
    (loop [i 0]
      (if (< i (count features))
         (let [feature (get features i)]
           (if (get feature "geometry")
             (drawGeometry state (get feature "geometry")))
           (recur (inc i)))))))

(defn drawPath [path]
  (loop [i 0]
    (if (< i (count path))
      (let [point (get path i)]
        (drawPoint point)
        (recur (inc i))))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (drawCircles)
  (q/stroke 100 255 255)
  (drawFeatures state (:geo state))
  (q/stroke 0 0 0)
  (drawPath path)
)
