(defpackage :sketch (:use :cl :trivial-gamekit :black-tie))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "2D Perlin noise"))

(defun get-color-for-pos (x y)
  (let* ((gray (+ 0.5 (perlin-noise-ref (* x 0.01) (* y 0.01) 0.0f0))))
    (vec4 gray gray gray 1)))

(defmethod draw ((this sketch))
  (loop for x from 0 to *width* do
        (loop for y from 0 to *height*
              do (draw-rect (vec2 x y) 1 1
                            :fill-paint (get-color-for-pos x y)))))

(start 'sketch)
