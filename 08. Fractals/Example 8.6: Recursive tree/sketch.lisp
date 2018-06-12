(defpackage :nature-of-code.fractals.example-8.6
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge))
(in-package :nature-of-code.fractals.example-8.6)

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

(defun draw-black-line (origin end)
  (draw-line origin end *black* :thickness 3))

(defun branch (len)
  (draw-black-line (vec2 0 0) (vec2 0 len))
  (translate-canvas 0 len)

  (setf len (* 0.66 len))
  (when (> len 2)
    (with-pushed-canvas ()
      (rotate-canvas (- (/ pi 6)))
      (branch len))

    (with-pushed-canvas ()
      (rotate-canvas (/ pi 6))
      (branch len))))

(defmethod draw ((this sketch))
  (translate-canvas *center-x* 0)
  (branch 100))

(defmethod act ((this sketch)))

(defun start-sketch ()
  (start 'sketch))
