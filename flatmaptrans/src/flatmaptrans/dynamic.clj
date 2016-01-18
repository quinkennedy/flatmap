(ns flatmaptrans.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [flatmaptrans.common :as c]))

(defn setup []
  (q/frame-rate 1)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:geo (json/read (io/reader "/media/quin/data/docs/me/flatmap/export.geojson"))})

(def path [[91.63 22.57] [91.58 22.615] [91.5 22.7] [91.45 22.73] [91.4 22.74] [91.35 22.74] [91.3 22.73] [91.28 22.7] [91.28 22.65]])

(defn flattenPath [path]
  (loop [flat [(first path)] i 1]
    (if (< i (count path))
      (recur 
        (conj flat 
              [(- (first (last flat)) 
                  (c/getDistance (get path i) 
                               (get path (dec i)))) 
               (second (first flat))])
        (inc i))
      flat)))

(def flatpath (flattenPath path))

(defn getPathTangents [path]
  (loop [i 0 tangents []]
    ;if this is the first point
    (if (= i 0)
      (recur (inc i) 
             (conj tangents 
                   (second (apply c/CtoP 
                                  (c/moveFromTo (get path (inc i)) 
                                                (get path i) 
                                                [0 0])))))
      ;else if this is the last point
      (if (= i (dec (count path)))
        (conj tangents
              (second (apply c/CtoP 
                             (c/moveFromTo (get path i)
                                           (get path (dec i))
                                           [0 0]))))
        ;else a point in the middle
        (recur (inc i)
               (conj tangents
                     (/ (+
                          (second (apply c/CtoP 
                                         (c/moveFromTo (get path (inc i))
                                                       (get path i)
                                                       [0 0])))
                          (second (apply c/CtoP 
                                         (c/moveFromTo (get path i)
                                                       (get path (dec i))
                                                       [0 0])))) 2)))))))

(def pathTangents (getPathTangents path))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:geo (:geo state)})

(defn offset [point x]
  [(+ (first point) x) (second point)])

(defn trackPath [point path flatpath placementFn]
  (let [polars (mapv #(apply c/CtoP (c/moveFromTo point % [0 0])) path)
        rotated (mapv (fn [polar tangent] 
                        [(first polar) 
                         (+ (second polar) (- Math/PI tangent))]) 
                      polars pathTangents)
        destinations (mapv (fn [rotated flatpath]
                             (c/moveFromTo (apply c/PtoC rotated)
                                           [0 0]
                                           flatpath)) 
                           rotated 
                           flatpath)]
    (placementFn polars destinations)))
;    (loop [minI 0 i 1]
;      ;if i is in range
;      (if (< i (count path))
;        ;then
;        ;if the current min is still smaller
;        (if (< (first (get polars minI)) (first (get polars i)))
;          (recur minI (inc i))
;          (recur i (inc i)))
;        ;else
;        ;translate point from path to flatpath based on closest point
;        (get destinations minI)))))
;        ;(c/moveFromTo (apply c/PtoC (get rotated minI)) 
;                      ;[0 0] 
;                      ;(get flatpath minI))))))

(defn trackByMin [polars destinations]
  (second (first (sort-by #(first (first %)) (map vector polars destinations)))))

(defn trackByStraightAvg [polars destinations]
  (mapv #(/ % (count destinations)) 
        (mapv #(apply + %) 
              (apply mapv vector destinations))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (q/stroke 100 255 255)
  (c/drawFeatures state 
                  (:geo state) 
                  #(c/positionPoint %))
  (q/stroke 0 255 255)
  (c/drawFeatures state 
                  (:geo state) 
                  #(c/positionPoint 
                      (trackPath  % 
                                  path
                                  flatpath
                                  trackByStraightAvg)))
  (q/stroke 0 0 0)
  (c/drawPath path #(c/positionPoint %))
  (c/drawPath flatpath #(c/positionPoint %)))
