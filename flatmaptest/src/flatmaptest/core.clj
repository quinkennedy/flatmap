(ns flatmaptest.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [flatmaptest.dynamic :as dynamic]))

(q/defsketch flatmaptest
  :title "You spin my circle right round"
  :size [1000 500]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :draw dynamic/draw-state
  :mouse-click dynamic/mouse-click
  :mouse-down dynamic/mouse-click
;  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
