(defpackage :nature-of-code.introduction.exercise-4
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :alexandria)
  (:shadowing-import-from :alexandria :lerp))
(in-package :nature-of-code.introduction.exercise-4)

(defvar *width* 800)
(defvar *height* 600)
(defvar *number-of-dots* 20)
(defvar *color-base* (vec4 0 1 0.2 1)
  "The base colour around which the paint will be randomized")

(defun gaussian-random-with (sd mean)
  "Helper function to get a random gaussian value with the given sd and mean."
  (+ (* sd (gaussian-random)) mean))

(defun random-position ()
  (vec2
   (gaussian-random-with (/ *width* 10) (/ *width* 2))
   (gaussian-random-with (/ *height* 10) (/ *height* 2))))

(defun bounded-gaussian-random (sd mean min max)
  "Generates a 'random' number, ensuring it's between min and max."
  (let ((r (gaussian-random-with sd mean)))
    (if (and (>= r min) (<= r max))
        r
        (bounded-gaussian-random sd mean min max))))

(defun random-color (base)
  "Random color similar to base."
  (flet ((random-rgb (n)
           (bounded-gaussian-random 0.1 n 0 1)))
    (vec4
     (random-rgb (x base))
     (random-rgb (y base))
     (random-rgb (z base))
     1)))

(defclass dot ()
  ((pos
   :initform (random-position)
   :accessor pos)
   (radius
    :initform 50
    :accessor radius)
   (color
    :initform (random-color *color-base*)
    :accessor color)))

(defmethod draw ((dot dot))
  (draw-circle (pos dot) (radius dot) :fill-paint (color dot)))

(defgame sketch ()
  ((dots
    :initform (loop for i to *number-of-dots* collect (make-instance 'dot))
    :accessor dots))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Gaussian paint"))

(defmethod draw ((this sketch))
  (loop for dot in (dots this) do (draw dot)))

(defun start-sketch ()
  (start 'sketch))
