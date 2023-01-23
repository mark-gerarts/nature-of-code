(defpackage :nature-of-code.fractals.example-8.3
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.fractals.example-8.3)

(defvar *width* 640)
(defvar *height* 360)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defun draw-recursive-circles (origin radius)
  (draw-circle origin radius :stroke-paint *black*)
  (when (> radius 2)
    (let ((r/2 (/ radius 2)))
      (draw-recursive-circles (vec2 (- (x origin) radius) (y origin)) r/2)
      (draw-recursive-circles (vec2 (+ (x origin) radius) (y origin)) r/2)
      (draw-recursive-circles (vec2 (x origin) (+ (y origin) radius)) r/2)
      (draw-recursive-circles (vec2 (x origin) (- (y origin) radius)) r/2))))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Recursion four times"))

(defmethod draw ((this sketch))
  (draw-recursive-circles (vec2 *center-x* *center-y*) 200))

(defun start-sketch ()
  (start 'sketch))
