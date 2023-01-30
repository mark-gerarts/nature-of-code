(defpackage :nature-of-code.oscillation.exercise-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.oscillation.exercise-1)

(defvar *width* 800)
(defvar *height* 200)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defgame sketch ()
  ((rotation
    :initform 0
    :accessor rotation))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Rotation"))

(defmethod draw ((this sketch))
  (labels ((draw-baton-circle (x)
             (draw-circle (vec2 x 0) 15
                          :fill-paint *gray*
                          :stroke-paint *black*
                          :thickness 2.0)))
    (translate-canvas (/ *width* 2) (/ *height* 2))
    (rotate-canvas (rotation this))
    (draw-line (vec2 -50 0) (vec2 50 0) *black* :thickness 2.0)
    (draw-baton-circle -50)
    (draw-baton-circle 50)))

(defmethod act ((this sketch))
  (with-accessors ((rotation rotation)) this
    (incf rotation -0.05)
    (setf rotation (mod rotation (* 2 pi)))))

(defun start-sketch ()
  (start 'sketch))
