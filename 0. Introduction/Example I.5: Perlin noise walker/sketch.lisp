(defpackage :nature-of-code.introduction.example-5
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :black-tie))
(in-package :nature-of-code.introduction.example-5)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun perlin-noise-1d (dt axis)
  "Perlin noise for the given axis. Ranges from 0 to 1."
  (let ((r (case axis
             (x (perlin-noise-ref dt 0.0f0 0.0f0))
             (y (perlin-noise-ref 0.0f0 dt 0.0f0))
             (z (perlin-noise-ref 0.0f0 0.0f0 dt)))))
    ;; Add 0.5, because perlin-noise-ref ranges from -0.5 to 0.5.
    (+ r 0.5)))

(defclass walker ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (tx
    :initform 0
    :accessor tx)
   (ty
    :initform 0
    :accessor ty)))

(defmethod step-walker ((this walker))
  (let ((rx (perlin-noise-1d (tx this) 'y))
        (ry (perlin-noise-1d (ty this) 'x)))
    (setf (pos this) (vec2 (* rx *width*) (* ry *height*)))
    (incf (tx this) 0.005)
    (incf (ty this) 0.005)))

(defmethod draw ((this walker))
  (draw-circle (pos this) 25
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((walker
    :initform (make-instance 'walker)
    :accessor walker))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Perlin noise walker"))

(defmethod draw ((this sketch))
  (draw (walker this)))

(defmethod act ((this sketch))
  (step-walker (walker this)))

(defun start-sketch ()
  (start 'sketch))
