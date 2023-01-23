(defpackage :nature-of-code.introduction.exercise-8
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :black-tie))
(in-package :nature-of-code.introduction.exercise-8)

(defvar *width* 800)
(defvar *height* 600)

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "2D Perlin noise - colour"))

(defun random-color-value (x y offset)
  (+ 0.5 (perlin-noise-ref (* x offset) (* y offset) 0.0f0)))

(defun get-color-for-pos (x y)
  (let ((r (random-color-value x y 0.1))
        (g (random-color-value x y 0.2))
        (b (random-color-value x y 0.3)))
    (vec4 r g b 1)))

(defmethod draw ((this sketch))
  (loop for x from 0 to *width* do
    (loop for y from 0 to *height*
          do (draw-rect (vec2 x y) 1 1
                        :fill-paint (get-color-for-pos x y)))))

(defun start-sketch ()
  (start 'sketch))
