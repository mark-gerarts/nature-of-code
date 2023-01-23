(defpackage :nature-of-code.fractals.exercise-8.1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.fractals.exercise-8.1)

(defvar *width* 600)
(defvar *height* 600)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defun draw-hexagon (center r)
  (let* ((xc (x center))
         (yc (y center))
         (vertices (loop for i upto 6
                         collect (let ((angle (* i (/ pi 3))))
                                   (vec2 (+ xc (* r (cos angle)))
                                         (+ yc (* r (sin angle))))))))
    (draw-polygon vertices :fill-paint *black*)))

(defun draw-tilted-hexagon (center r)
  (with-pushed-canvas ()
    (translate-canvas (x center) (y center))
    (rotate-canvas (/ pi 6))
    (draw-hexagon (vec2 0 0) r)))

(defun recursive-hexes (origin r &optional (scale 0.5) (spacing 4))
  (draw-tilted-hexagon origin r)
  (when (> r 10)
    (let ((x (x origin))
          (y (y origin))
          (scaled-r (* r scale)))
      (recursive-hexes (vec2 x (+ y (* spacing scaled-r))) scaled-r)
      (with-pushed-canvas ()
        (translate-canvas (x origin) (y origin))
        (rotate-canvas (/ pi 1.5))
        (recursive-hexes (vec2 0 (* spacing scaled-r)) scaled-r))
      (with-pushed-canvas ()
        (translate-canvas (x origin) (y origin))
        (rotate-canvas (- (/ pi 1.5)))
        (recursive-hexes (vec2 0 (* spacing scaled-r)) scaled-r)))))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Custom recursion"))

(defmethod draw ((this sketch))
  (recursive-hexes (vec2 *center-x* *center-y*) 75))

(defun start-sketch ()
  (start 'sketch))
