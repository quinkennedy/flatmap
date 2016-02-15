(ns nearby-path-extractor.core
  (:require [clojure.java.io :as io])
  (:import [org.geotools.data.shapefile ShapefileDataStore])
  (:gen-class))

(def shapefilepath
  "file:///media/quin/data1/docs/me/flatmap/coastline/land-polygons-split-3857/land_polygons.shp")
(def pathpath
  "/media/quin/data1/docs/me/flatmap/path.txt")
(def pathWidth 1000000)
(def path (read-string (slurp pathpath)))
(def shapefile (ShapefileDataStore.
                 (io/as-url shapefilepath)))
(def featureReader (.getFeatureReader shapefile))

(defn geom-seq [featureReader]
  (when (.hasNext featureReader)
    (let [coords (.getCoordinates
                   (.getAttribute
                     (.next featureReader) 0))]
      (cons (map (fn coodToList [coord]
                   (list (.x coord) (.y coord)))
                 coords)
            (lazy-seq (geom-seq featureReader))))))

(defn getLength [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn getDistance [p1 p2]
  (getLength (map - p1 p2)))

(defn isGeomClose [geom]
  (reduce
    (fn closeCoord [prev coord]
      (or prev
          (reduce
            (fn closePath [prev point]
              (or prev
                  (>= pathWidth
                      (getDistance point
                                   coord))))
            false
            path)))
    false
    geom))

(def closeEnoughGeoms (filter isGeomClose (geom-seq featureReader)))

(defn saveData [filepath data]
  (spit filepath (with-out-str (pr data))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (saveData "/media/quin/data1/docs/me/flatmap/geoms.txt"
            closeEnoughGeoms)
  (println "done"))
