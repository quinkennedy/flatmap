(ns flatmapshape.common
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn getDistance [pointA pointB]
  (let [xDiff (- (first pointB) (first pointA))
        yDiff (- (second pointB) (second pointA))]
    (Math/sqrt (+ (* xDiff xDiff) (* yDiff yDiff)))))

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

(defn PtoC [r a]
  [(* (Math/cos a) r) (* (Math/sin a) r)])

(defn CtoP [x y]
  [(getDistance [0 0] [x y])
   (getAngle [0 0] [x y])])

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

(defn positionPoint [point]
  (let [divisor 9
		xScale (/ 10000 divisor)
		yScale (- 0 xScale)
        source [91.64 22.57]
        destination [(* (q/width) (/ 3 4)) (/ (q/height) 2)]]
    (mapv + (mapv * (mapv - point source) [xScale yScale]) destination)))

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

(defn drawPoint [point func]
        ;ppoint (positionPoint point)
        ;apoint (adjustPoint ppoint)
		;npoint (straightenFullPath apoint (map adjustPoint (map positionPoint path)))
    (apply q/point (func point)))
    
(defn drawGeometry [state geometry func]
  (let [points (get geometry "coordinates")]
    (if points
      (loop [i 0]
        (if (< i (count points))
          (let [point (get points i)]
            (drawPoint point func)
            (recur (inc i))))))))

(defn drawFeatures [state geo func]
  (let [features (get geo "features")]
    (loop [i 0]
      (if (< i (count features))
         (let [feature (get features i)]
           (if (get feature "geometry")
             (drawGeometry state (get feature "geometry") func))
           (recur (inc i)))))))

(defn drawPath [path func]
  (loop [i 0]
    (if (< i (count path))
      (let [point (get path i)]
        ;(drawPoint point func)
        (apply q/ellipse (conj (func point) 5 5))
        (recur (inc i))))))
