(ns flatmapshape.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [flatmapshape.dynamic :as dynamic]))

(q/defsketch flatmapshape
  :title "map from shapefile"
  :size [800 800]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :draw dynamic/draw-state
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn refresh []
  (use :reload 'flatmapshape.dynamic)
  (.loop flatmapshape))
