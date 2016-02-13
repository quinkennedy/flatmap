(defproject flatmapclick "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"osgeo-geotools" "http://download.osgeo.org/webdav/geotools"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [quil "2.3.0"]
                 ; jai_core to avoid "MathTransformProvider" warning
                 [javax.media/jai_core "1.1.3"]
                 [org.geotools/gt-shapefile "2.7-M3"]])
