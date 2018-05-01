(defpackage :sketch (:use :cl :trivial-gamekit))
(in-package :sketch)

(defvar *width* 640)
(defvar *height* 240)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun draw-square (i value)
  "Draws a single square of the distribution."
  (let* ((width (/ *width* 20))
         (offset (* i width)))
    (draw-rect (vec2 offset 0)
               width
               value
               :fill-paint *gray*
               :stroke-paint *black*)))

(defun draw-distribution (distribution)
  "Draws the entire distribution."
  (loop for i from 0
        for n across distribution
        do (draw-square i n)))

(defgame sketch ()
  ((numbers
    :initform (make-array 20 :initial-element 0)
    :accessor numbers))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Random number distribution"))

(defmethod post-initialize ((this sketch)))

(defmethod draw ((this sketch))
  (draw-distribution (numbers this)))

(defmethod act ((this sketch))
  (incf (elt (numbers this) (random 20))))

(start 'sketch)
