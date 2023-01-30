(defpackage :nature-of-code.fractals.example-8.4
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.fractals.example-8.4)

(defvar *width* 800)
(defvar *height* 200)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defun cantor (origin length)
  (when (> length 1)
    (let* ((l/3 (/ length 3))
           (x (x origin))
           (y (y origin))
           (new-y (- y 25)))
      (draw-line origin (vec2 (+ x length) y) *black* :thickness 10)
      (cantor (vec2 x new-y) l/3)
      (cantor (vec2 (+ x (* l/3 2)) new-y) l/3))))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Cantor set"))

(defmethod draw ((this sketch))
  (cantor (vec2 10 (- *height* 20)) (- *width* 20)))

(defun start-sketch ()
  (start 'sketch))
