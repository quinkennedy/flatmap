(ns flatmapclick.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [flatmapclick.dynamic :as d]))
(q/defsketch flatmapclick
  :title "You spin my circle right round"
  :size [800 800]
  ; setup function called only once, during sketch initialization.
  :setup d/setup
  ; update-state is called on each iteration before draw-state.
  :update d/update-state
  :draw d/draw-state
  :mouse-clicked d/mouse-clicked
  :key-typed d/key-typed
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
