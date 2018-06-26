(defpackage :nature-of-code.fractals.example-8.1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.fractals.example-8.1)

(defvar *width* 600)
(defvar *height* 400)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defun draw-recursive-circles (origin radius)
  (draw-circle origin radius :stroke-paint *black*)
  (when (> radius 2)
    (draw-recursive-circles origin (* 0.75 radius))))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Recursive circles I"))

(defmethod draw ((this sketch))
  (draw-recursive-circles (vec2 *center-x* *center-y*) *width*))

(defun start-sketch ()
  (start 'sketch))
