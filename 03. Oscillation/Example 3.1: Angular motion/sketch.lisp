(defpackage :nature-of-code.oscillation.example-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.oscillation.example-1)

(defvar *width* 800)
(defvar *height* 200)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defgame sketch ()
  ((angle
    :initform 0
    :accessor angle)
   (a-velocity
    :initform 0
    :accessor a-velocity)
   (a-acceleration
    :initform 0.001
    :accessor a-acceleration))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Angular motion"))

(defmethod draw ((this sketch))
  (translate-canvas (/ *width* 2) (/ *height* 2))
  (rotate-canvas (angle this))
  (draw-line (vec2 -50 0) (vec2 50 0) *black* :thickness 2.0)
  (draw-circle (vec2 50 0) 15 :fill-paint *gray* :stroke-paint *black* :thickness 2.0)
  (draw-circle (vec2 -50 0) 15 :fill-paint *gray* :stroke-paint *black* :thickness 2.0))

(defmethod act ((this sketch))
  (with-accessors ((angle angle)
                   (a-velocity a-velocity)
                   (a-acceleration a-acceleration)) this
    (setf a-velocity (+ a-velocity a-acceleration))
    (setf angle (+ angle a-velocity))))

(defun start-sketch ()
  (start 'sketch))
