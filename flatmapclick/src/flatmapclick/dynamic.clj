(ns flatmapclick.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [flatmapclick.common :as c]
            [clojure.java.io :as io])
  (:import  [org.geotools.data.shapefile ShapefileDataStore]))

(defn loadData [path]
  (read-string (slurp path)))

(def shapefilepath
     "file:///media/quin/data1/docs/me/flatmap/coastline/land-polygons-split-3857/land_polygons.shp")
(def defaultZoom ['(-20000000 20000000) '(20000000 -20000000)])
(def pathWidth 1000000)

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
     ;:shapefile shapefile
     ;:reader (.getFeatureReader shapefile)
     :zoom defaultZoom
     :zoomtoggle false
     :pathtoggle false
     :drawing true
     :frame 1
     :allgeoms (loadData "/media/quin/data1/docs/me/flatmap/geoms.txt")
     :currgeoms '()
     :path (loadData "/media/quin/data1/docs/me/flatmap/path.txt")
     :line (loadData "/media/quin/data1/docs/me/flatmap/line.txt")
     :hiddenUI false
     :clickfunc "default"
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

(defn saveData [filepath data]
  (spit filepath (with-out-str (pr data))))

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
    ; P key toggles click-to-define-path
    \P (merge state {:clickfunc (if (= "path" (:clickfunc state))
                                    "default"
                                    "path")})
    ; L key toggles click-to-add-line
    \L (merge state {:clickfunc (if (= "line" (:clickfunc state))
                                    "default"
                                    "line")})
    ; s key saves zoom/path/line
    \s (do 
         (case (:clickfunc state)
		   "path" (saveData 
                    "/media/quin/data1/docs/me/flatmap/path.txt" 
                    (:path state))
           "line" (saveData
                    "/media/quin/data1/docs/me/flatmap/line.txt"
                    (:line state))
           "default" (saveData
                       "/media/quin/data1/docs/me/flatmap/zoom.txt"
                       (:zoom state)))
         state)
    ; u key removes last path (or line) point
    \u (do
         (q/debug (str "path: " (vec (drop-last (:path state)))))
         (merge state {:path (vec (drop-last (:path state)))}))
    ; d key toggle drawing
    \d (merge state {:drawing (not (:drawing state))})
    ; r key resets (starts drawing from beginning)
    \r (do
         (q/debug "resetting")
         ;(.close (:reader state))
         (merge state {:color 0
                       :nextgeoms (:allgeoms state)
                       :drawing  true
         ;              :reader   (.getFeatureReader 
         ;                          (:shapefile state))
                       :frame    0}))
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

(def minLength 10000)

(defn rrange [a b c]
    (if (< b a)
      (map - (repeat 0) (range (- 0 a) (- 0 b) (- 0 c)))
      (range a b c)))

(defn normalize [v]
  (let [d (c/getDistance c/origin v)]
    (if (= 0 d)
      v
      (map / v (repeat d)))))

(defn interpolate [p1 p2]
  (let [d (c/getDistance p1 p2)]
    (if (>= minLength d)
      (list p1)
      (let [ranges (map rrange p1 
                               p2 
                               (map * 
                                    (normalize (map - p2 p1))
                                    (repeat minLength)))
            nums   (apply max (map count ranges))
            equalRanges (map (fn [rng end]
                               (take nums (concat rng (repeat end))))
                             ranges
                             p2)]
  ; I decided not to include p2 explicitly because
  ; it will be included at the beginning of the next
  ; interpolation.
        (apply map 
               vector
               equalRanges)))
))

(defn interpolateCoords [coords]
  (concat (mapcat #(apply interpolate %)
                  (map vector
                       coords
                       (rest coords)))
          (list (last coords))))

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
              geoms)))

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
                                       (trackByTwoTween
                                         state
                                         coord))))
                            (interpolateCoords coords)))
                (q/end-shape))
              (filter #(bigEnough state 
                                  (getBoundingBox %)) 
                      (:currgeoms state)))))

(def grouping 100)

(defn update-state [state]
  (merge state 
         {:color (mod (+ (:color state) 0.7) 255)
          :drawing true
          :nextgeoms (if (contains? state :nextgeoms)
                         (drop grouping (:nextgeoms state))
                         (:allgeoms state))
          :currgeoms (take grouping (:nextgeoms state))
          :frame (inc (:frame state))
         }))

(defn draw-state [state]
  ; draw map on off-screen buffer
  (when (< 0 (count (:currgeoms state)))
    (q/debug (str "f: " (:frame state)))
    (q/with-graphics
      (:mapLayer state)
      (if (= 1 (:frame state))
        (q/background 0 0 0 0))
      (q/with-fill 
        [0 153 0];(:color state) 255 255]
        (q/with-stroke
          [0 153 0]
          (drawGeoms state))))
    (if (= 0 (count (:nextgeoms state)))
      (q/debug "done")))
  (q/background 130 255 255)
  ; draw map on-screen
  (q/image (:mapLayer state) 0 0)
  ;;draw path
  ;(if (< 0 (count (:path state)))
  ;  (let [path (if (:pathtoggle state)
  ;                 (c/flattenPath (:path state))
  ;                 (:path state))
  ;        screenPath (map #(map-to-screen state %) path)]
  ;    (dorun (reduce (fn [a b] (q/line (first a) 
  ;                                     (second a) 
  ;                                     (first b) 
  ;                                     (second b))
  ;                             b)
  ;                   screenPath))
  ;  ))
  ;draw line
  (if (< 0 (count (:line state)))
    (let [line (if (:pathtoggle state)
                   (map #(trackByTwoTween state %) (:line state))
                   (:line state))
          screenLine (map #(map-to-screen state %) line)]
      (dorun 
        (map #(apply q/line %) 
             (map concat 
                  screenLine 
                  (rest screenLine))))))
  ; display text
  (if (not (:hiddenUI state))
    (q/with-fill
      [0]
      (q/text (:clickfunc state)
              10
              20)))
)




