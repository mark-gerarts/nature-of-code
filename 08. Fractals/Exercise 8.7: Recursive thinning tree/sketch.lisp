(defpackage :nature-of-code.fractals.exercise-8.7
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge))
(in-package :nature-of-code.fractals.exercise-8.7)

(defvar *width* 600)
(defvar *height* 400)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Recursive tree"))

(defun branch (len &optional (theta (/ pi 6)) (thickness 8))
  (draw-line (vec2 0 0) (vec2 0 len) *black* :thickness thickness)
  (translate-canvas 0 len)

  (setf len (* 0.66 len))
  (setf thickness (* 0.8 thickness))

  (when (and (> len 2) (> thickness 0))
    (with-pushed-canvas ()
      (rotate-canvas (- theta))
      (branch len theta thickness))

    (with-pushed-canvas ()
      (rotate-canvas theta)
      (branch len theta thickness))))

(defmethod draw ((this sketch))
  (translate-canvas *center-x* 0)
  (branch 100))

(defmethod act ((this sketch)))

(defun start-sketch ()
  (start 'sketch))
