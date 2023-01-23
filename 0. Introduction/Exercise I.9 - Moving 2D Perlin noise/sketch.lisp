(defpackage :nature-of-code.introduction.exercise-9
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :black-tie))
(in-package :nature-of-code.introduction.exercise-9)

(defvar *width* 630)
(defvar *height* 240)

(defgame sketch ()
  ((dz
    :initform 0
    :accessor dz))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Moving 2D Perlin noise"))

(defun get-color-for-pos (x y z)
  (let* ((gray (+ 0.5 (perlin-noise-ref (* x 0.01) (* y 0.01) z))))
    (vec4 gray gray gray 1)))

(defmethod draw ((this sketch))
  (loop for x from 0 to *width* do
    (loop for y from 0 to *height*
          do (draw-rect (vec2 x y) 1 1
                        :fill-paint (get-color-for-pos x y (dz this))))))

(defmethod act ((this sketch))
  (incf (dz this) 0.02))

(defun start-sketch ()
  (start 'sketch))
