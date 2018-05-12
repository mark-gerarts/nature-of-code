(defpackage :nature-of-code.vectors.exercise-6
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :black-tie)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.vectors.exercise-6)

(defvar *width* 640)
(defvar *height* 360)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun perlin-acceleration (dt)
  (let ((x (perlin-noise-ref dt 0.0f0 0.0f0))
        (y (perlin-noise-ref 0.0f0 dt 0.0f0)))
    (vec2 x y)))

(defclass mover ()
  ((location
    :initform (vec2 (random-in-range 0 *width*) (random-in-range 0 *height*))
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (top-speed
    :initform 10
    :accessor top-speed)))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location)))
    (cond ((< x 0) (setf (x location) *width*))
          ((> x *width*) (setf (x location) 0)))
    (cond ((< y 0) (setf (y location) *height*))
          ((> y *height*) (setf (y location) 0)))))

(defmethod update ((mover mover) dt)
  (let* ((a (perlin-acceleration dt))
         (v (limit-vec (add a (velocity mover)) 10)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))
    (check-edges mover)))

(defmethod display ((mover mover))
  (draw-circle (location mover) 16
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((mover
    :initform (make-instance 'mover)
    :accessor mover)
   (dt
    :initform 0.0
    :accessor dt))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Motion 101 (velocity and random acceleration)"))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update (mover this) (dt this))
  (incf (dt this) 0.005))

(defun start-sketch ()
  (start 'sketch))
