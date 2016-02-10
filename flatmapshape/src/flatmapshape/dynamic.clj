(ns flatmapshape.dynamic
  (:require [quil.core :as q]
            [flatmapshape.common :as c]
            [clojure.java.io :as io])
  (:import
            [org.geotools.data.shapefile ShapefileDataStore]))

(defn setup []
  (let [url (io/as-url "file:///media/quin/data1/docs/me/flatmap/coastline/land-polygons-split-3857/land_polygons.shp")
        shp (ShapefileDataStore. url)]
  (q/debug (.next (.getFeatureReader shp)))
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :shapefile shp
   :reader (.getFeatureReader shp)
   :frame 0}))

(defn update-state [state]
  (q/debug (:frame state))
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :shapefile (:shapefile state)
   :reader (:reader state)
   :geoms (loop [i 0 geoms []]
            (if (and (< i 2000) (.hasNext (:reader state)))
              (recur (inc i) 
                     (conj geoms 
                           (.getCoordinates 
                             (.getAttribute 
                               (.next (:reader state)) 0))))
              geoms))
   :frame (inc (:frame state))})

(def minX -20000000)
(def minY -20000000)
(def swath 40000000)

(defn draw-state [state]
  (if (= 0 (count (:geoms state)))
      (q/no-loop)
      (let []
  ; Clear the sketch by filling it with light-grey color.
  ;(q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
;  ; Calculate x and y coordinates of the circle.
;  (let [angle (:angle state)
;        x (* 150 (q/cos angle))
;        y (* 150 (q/sin angle))]
;    ; Move origin point to the center of the sketch.
;    (q/with-translation [(/ (q/width) 2)
;                         (/ (q/height) 2)]
;      ; Draw the circle.
;      (q/ellipse x y 100 100)))

;  (loop [reader (.getFeatureReader (:shapefile state))
;         i 0]
;    (if (and (.hasNext reader) (< i 5000))
;      (let [coords (.getCoordinates (.getAttribute (.next reader) 0))]
        (dorun (map (fn drawGeom [coords] 
                      (let [bb
                               (loop [minX (.x (get coords 0))
                                      miny (.y (get coords 0))
                                      maxX minX
                                      maxY minY
                                      i    1]
                                 (if (< i (count coords))
                                     (let [coord (get coords i)]
                                       (recur (min minX (.x coord))
                                              (min minY (.y coord))
                                              (max maxX (.x coord))
                                              (max maxY (.y coord))
                                              (inc i)))
                                     {:x minX 
                                      :y minY 
                                      :w (- maxX minX) 
                                      :h (- maxY minY)}))]
                        (if (and (>= (:w bb) (/ swath (q/width)))
                                 (>= (:h bb) (/ swath (q/height))))
                          (let []
                            (q/begin-shape)
                            (dorun (map (fn setVert
                                            [coord]
                                            (apply q/vertex 
                                                   (map q/map-range
                                                        [(.x coord) 
                                                         (.y coord)]
                                                        [minX 
                                                         (+ minY swath)] 
                                                        [(+ minX swath)
                                                         minY]
                                                        [0 0] 
                                                        [(q/width) 
                                                         (q/height)])))
                                         coords))
                            (q/end-shape))))) 
                    (:geoms state)))
;        (recur reader (inc i)))))
)))

  ;(q/debug (reduce (fn [a b] (map min a b)) (map #(reduce (fn [a b] (map min a b)) %) (:geoms state))))
  ;(q/debug (reduce (fn [a b] (map max a b)) (map #(reduce (fn [a b] (map max a b)) %) (:geoms state))))
;  (dorun (map (fn doShape 
;                  [geom]
;                  (q/begin-shape)
;                  (dorun (map (fn setVert 
;                                  [g] 
;                                  (apply q/vertex (map q/map-range
;                                              g 
;                                              [minX, minY] 
;                                              [(+ minX swath), (+ minY swath)]
;                                              [0, 0] 
;                                              [(q/width), (q/height)])))
;                              geom))
;                  (q/end-shape))
;              (:geoms state))))
