(ns flatmapclick.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [flatmapclick.common :as c]
            [clojure.java.io :as io])
  (:import  [org.geotools.data.shapefile ShapefileDataStore]))

(def shapefilepath
     "file:///media/quin/data1/docs/me/flatmap/coastline/land-polygons-split-3857/land_polygons.shp")
(def defaultZoom ['(-20000000 20000000) '(20000000 -20000000)])
(def startGeom 0);(* 275 2000))
(def pathWidth 1000000)
(def seaPath ['(12676500 2499500.0) '(12596438 2350000.0) '(12516375 2212000.0) '(12459188 2062500.0) '(12402000 1855500.0) '(12367688 1591000.0) '(12321938 1326500.0) '(12241875 1073500.0) '(12127500 820500.0) '(12013125 590500.0) '(11910188 360500.0) '(11795813 130500.0) '(11704312 27000.0) '(11635688 38500.0) '(11578500 61500.0) '(11498438 119000.0) '(11395500 211000.0) '(11258250 314500.0) '(11143875 418000.0) '(10949438 648000.0) '(10743562 958500.0) '(10549125 1326500.0) '(10434750 1591000.0) '(10286062 1970500.0) '(10137375 2212000.0) '(10011562 2315500.0) '(9874312.0 2384500.0) '(9805688.0 2384500.0) '(9759938.0 2373000.0) '(9714188.0 2350000.0) '(9714188.0 2292500.0) '(9702750.0 2200500.0) '(9576938.0 1959000.0) '(9428250.0 1763500.0) '(9279562.0 1533500.0) '(9096562.0 1315000.0) '(8925000.0 1142500.0) '(8890688.0 1119500.0) '(8856375.0 1062000.2) '(8833500.0 1027500.0) '(8810625.0 981500.0) '(8764875.0 889500.0) '(8661938.0 809000.0) '(8559000.0 809000.0) '(8478938.0 843500.0) '(8433188.0 958500.0) '(8444625.0 1085000.0) '(8433188.0 1188500.0) '(8330250.0 1269000.0) '(8124375.0 1487500.0) '(7907062.5 1821000.0) '(7746937.5 2051000.0) '(7586812.5 2384500.0)'(7472437.5 2580000.0) '(7392375.0 2741000.0) '(7323750.0 2856000.0) '(7300875.0 2879000.0) '(7255125.0 2844500.0) '(7117875.0 2706500.0) '(6957750.0 2476500.0) '(6774750.0 2246500.0) '(6603187.5 2051000.0) '(6351562.5 1843999.9) '(6134250.0 1752000.0) '(5916937.5 1683000.0) '(5814000.0 1614000.0) '(5768250.0 1533500.0) '(5791125.0 1430000.0) '(5825437.5 1280500.0) '(5814000.0 1016000.0) '(5733937.5 762999.75) '(5550937.5 464000.0) '(5345062.5 222500.0) '(5104875.0 -42000.0) '(4818937.5 -272000.0) '(4407187.5 -536500.0)])


(defn setup []
  (let [shapefile (ShapefileDataStore.
                    (io/as-url shapefilepath))]
    ; Set frame rate to 30 frames per second.
    (q/frame-rate 30)
    ; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :hsb)
    ; setup function returns initial state. It contains
    ; circle color and position.
    {:color 0
     :shapefile shapefile
     :reader (.getFeatureReader shapefile)
     :zoom defaultZoom
     :zoomtoggle false
     :pathtoggle false
     :drawing true
     :frame 0
     :geomNum 0
     :path seaPath
     :mapLayer (q/create-graphics (q/width) (q/height))}))

(defn trackByTwoTween [state point]
  (if (not (:pathtoggle state))
    ;then
    point
    ;else
    (let [path (:path state)
          exPath (c/extendPath path)
          polars (mapv #(apply c/CtoP (c/moveFromTo point % c/origin)) path)
          pathTangents (c/getPathTangents path)
          flatpath (c/flattenPath path)
          rotated (mapv (fn [polar tangent]
                          [(first polar)
                           (+ (second polar) (- Math/PI tangent))])
                        polars
                        pathTangents)
          destinations (mapv (fn [rotated flatpath]
                               (c/moveFromTo (apply c/PtoC rotated)
                                             c/origin
                                             flatpath))
                             rotated
                             flatpath)
          sorted (sort-by #(first (first %))
                          (map vector polars destinations (range)))
          minI (last (first sorted))
          neighborMinI (last (first (filter #(= 1
                                               (Math/abs (- (last %)
                                                            minI)))
                                           (rest sorted))))
          neighborMinOppositeI (+ minI (- minI neighborMinI))
          neighborsPolar (apply c/CtoP
                                (c/moveFromTo (get path neighborMinI)
                                              (get exPath
                                                   (inc neighborMinOppositeI))
                                              c/origin))
          polarFromNMOI (apply c/CtoP
                               (c/moveFromTo point
                                             (get exPath
                                                  (inc neighborMinOppositeI))
                                             c/origin))
          distanceToNMOI (Math/abs
                           (first
                             (c/PtoC (first polarFromNMOI)
                                     (+ (second polarFromNMOI)
                                        (- Math/PI
                                           (second neighborsPolar))))))
          distanceToNeighborsLine (Math/abs
                                    (- distanceToNMOI
                                       (/ (first neighborsPolar) 2)))
          minsPolar (apply c/CtoP (c/moveFromTo (get path neighborMinI)
                                                (get path minI)
                                                c/origin))
          distanceToMin (Math/abs
                          (first
                            (c/PtoC (first (get polars minI))
                                    (+ (second (get polars minI))
                                       (- Math/PI
                                          (second minsPolar))))))
          distanceToMinLine (Math/abs
                              (- distanceToMin
                                 (/ (first minsPolar) 2)))
          addedDistance (+ distanceToMinLine distanceToNeighborsLine)]
      (mapv #(apply + %)
            (c/rowToCol [(mapv #(* (+ (/ (/ distanceToMinLine
                                            addedDistance)
                                         2)
                                      0.5)
                                   %)
                               (get destinations minI))
                         (mapv #(* (/ (/ distanceToNeighborsLine
                                         addedDistance)
                                      2)
                                   %)
                               (get destinations neighborMinI))])))))

(defn screen-to-map [state point]
  (let [zoom (if (:zoomtoggle state)
                 (:zoom state)
                 defaultZoom)]
    (map q/map-range
         point
         [0 0]
         [(q/width) (q/height)]
         (first zoom)
         (second zoom))))

(defn map-to-screen [state point]
  (let [zoom (if (:zoomtoggle state)
                 (:zoom state)
                 defaultZoom)]
    (map q/map-range
         point
         (first zoom)
         (second zoom)
         [0 0]
         [(q/width) (q/height)])))

(defn mouse-clicked [state event]
  (if (not (:zoomtoggle state))
    (let [newZoom (vec 
                    (rest 
                      (conj (:zoom state) 
                            (screen-to-map 
                              state 
                              [(:x event) (:y event)]))))]
      (q/debug (str "newZoom: " newZoom))
      (merge state {:zoom newZoom}))
    (let [newPath (conj (:path state)
                        (screen-to-map 
                          state 
                          [(:x event) (:y event)]))]
      (q/debug (str "newPath: " newPath))
      (merge state {:path newPath}))))

(defn key-typed [state event]
  (case (:raw-key event)
    ; z key toggles zoom
    \z (do
         (q/debug (str "zoom: " (not (:zoomtoggle state))))
         (merge state {:zoomtoggle (not (:zoomtoggle state))
                       :frame      0}))
    ; p key toggles warping along path
    \p (do
         (q/debug (str "path: " (not (:pathtoggle state))))
         (merge state {:pathtoggle (not (:pathtoggle state))
                       :frame      0}))
    ; u key removes last path point
    \u (do
         (q/debug (str "path: " (vec (drop-last (:path state)))))
         (merge state {:path (vec (drop-last (:path state)))}))
    ; d key toggle drawing
    \d (merge state {:drawing (not (:drawing state))})
    ; r key resets (starts drawing from beginning)
    \r (do
         (.close (:reader state))
         (merge state {:color 0
                       :drawing  true
                       :reader   (.getFeatureReader 
                                   (:shapefile state))
                       :frame    0
                       :geomNum 0}))
))

(defn getBoundingBox [coords]
  (reduce (fn [bb point]
            [[(min (first (first bb))
                   (first point))
              (min (second (first bb))
                   (second point))]
             [(max (first (second bb))
                   (first point))
              (max (second (second bb))
                   (second point))]])
          [[(first (first coords))
            (second (first coords))]
           [(first (first coords))
            (second (first coords))]]
          (rest coords)))

(defn getCoordBoundingBox [coords]
  (getBoundingBox (mapv #(vector (.x %) (.y %)) coords)))

(defn bigEnough [state bbox]
  ;(q/debug (str "s: " (vec (apply map - (map #(map-to-screen state %) (reverse bbox))))))
  (<= 1
      (apply min 
             (map #(Math/abs %)
                  (apply map 
                         - 
                         (map #(map-to-screen state %) 
                              (reverse bbox)))))))

(defn onScreen [state bbox]
  ;(q/debug (str "m: " (map #(map-to-screen state %) bbox)))
  (let [screenOnMap (mapv #(screen-to-map state %) [[0 0] [(q/width) (q/height)]])
        screenBBox  (getBoundingBox screenOnMap)]
    (and (<= (first (first screenBBox)) (first (second bbox)))
         (<= (second (first screenBBox)) (second (second bbox)))
         (>= (first (second screenBBox)) (first (first bbox)))
         (>= (second (second screenBBox)) (second (first bbox))))))

(defn geom-seq [featureReader]
  (when (.hasNext featureReader)
    (let [coords (.getCoordinates
                   (.getAttribute
                     (.next featureReader) 0))]
      (cons (map (fn coordToList [coord]
                   (list (.x coord) (.y coord)))
                 coords)
          (lazy-seq (geom-seq featureReader))))))

(defn filteredGeoms [geoms state]
  ;(q/debug (str "cd: " (first (first geoms))))
  (filter (fn visible [coords]
           (let [bbox (getBoundingBox coords)]
             ;(q/debug (str "b: " bbox))
             (and (bigEnough state bbox)
                  (onScreen state bbox))))
         (map (fn warpGeom [coords]
                (map (fn warpCoord [coord]
                       (trackByTwoTween state 
                                        coord))
                     coords))
              (filter 
                (fn closeGeom [coords]
                  (reduce 
                    (fn closeCoord [prev coord]
                      (or prev 
                          (reduce 
                            (fn closePathPt [prev point]
                              (or prev 
                                  (>= pathWidth
                                      (c/getDistance point
                                                     coord))))
                            false
                            (:path state))))
                    false
                    coords))
                geoms))))

(defn skipGeoms [reader amount]
  (loop [i 0]
    (if (and (< i amount) (.hasNext reader))
        (do
          (.next reader)
          (recur (inc i)))))
  reader)

(defn getGeoms [reader amount]
  (loop [i 0 geoms []]
    (if (and (< i amount) (.hasNext reader))
        (recur (inc i)
               (conj geoms
                     (.getCoordinates
                       (.getAttribute
                         (.next reader) 0))))
        geoms)))

(defn drawGeoms [state]
  (dorun (map (fn drawGeom [coords]
                ;(q/debug "drawGeom")
                (q/begin-shape)
                (dorun (map (fn setVert [coord]
                              ;(q/debug (str "v: " (map-to-screen state coord)))
                              (apply q/vertex 
                                     (map-to-screen 
                                       state 
                                       coord)))
                                      ; [(.x coord) 
                                      ;  (.y coord)])))
                            coords))
                (q/end-shape))
              (:geoms state))))
;              (filter (fn visible [coords]
;                        (let [bbox (getBoundingBox coords)]
;                          ;(q/debug (str "b: " bbox))
;                          (and (bigEnough state bbox)
;                               (onScreen state bbox))))
;                      (map (fn warpGeom [coords]
;                             (map (fn warpCoord [coord]
;                                    (trackByTwoTween state 
;                                                     coord))
;                                  coords))
;                           (filter 
;                             (fn closeGeom [coords]
;                               (reduce 
;                                 (fn closeCoord [prev coord]
;                                   (or prev 
;                                       (reduce 
;                                         (fn closePathPt [prev point]
;                                           (or prev 
;                                               (>= pathWidth
;                                                   (c/getDistance point
;                                                                  coord))))
;                                         false
;                                         (:path state))))
;                                 false
;                                 coords))
;                             (map #(map (fn coordToVec [coord] 
;                                          [(.x coord) (.y coord)])
;                                        %)
;                                  (:geoms state))))))))

(defn update-state [state]
  (merge state 
         {:color (mod (+ (:color state) 0.7) 255)
          :drawing (and (.hasNext (:reader state))
                        (:drawing state))
          :frame (inc (:frame state))
          :geomNum startGeom
          :geoms (take 100 (filteredGeoms (geom-seq (:reader state)) state))
          ;:geoms (if (and (:drawing state)
          ;                (.hasNext (:reader state)))
          ;           (getGeoms (if (< (:geomNum state) startGeom)
          ;                         (skipGeoms (:reader state) startGeom)
          ;                         (:reader state)) 
          ;                     (if (:pathtoggle state)
          ;                         200
          ;                         200))
          ;           [])
         }))

(defn draw-state [state]
  ; draw map on off-screen buffer
  (q/with-graphics
    (:mapLayer state)
    (if (= 1 (:frame state))
      (q/background 240))
    (q/with-fill 
      [(:color state) 255 255]
      (drawGeoms state)))
  ; draw map on-screen
  (q/image (:mapLayer state) 0 0)
  ;log frame number
  (if (:drawing state)
    (q/debug (str "f: " (:frame state))))
  ;draw path
  (if (< 0 (count (:path state)))
    (let [path (if (:pathtoggle state)
                   (c/flattenPath (:path state))
                   (:path state))
          screenPath (map #(map-to-screen state %) path)]
      (dorun (reduce (fn [a b] (q/line (first a) 
                                       (second a) 
                                       (first b) 
                                       (second b))
                               b)
                     screenPath))
    )))




