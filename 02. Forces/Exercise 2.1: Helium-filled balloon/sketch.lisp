(defpackage :nature-of-code.forces.exercise-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit :black-tie)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.exercise-1)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defun random-wind (dt)
  (let* ((x (perlin-noise-ref dt 0.0f0 0.0f0))
         (y (perlin-noise-ref 0.0f0 dt 0.0f0))
         (y (* y 0.1))) ; Make the wind more horizontal.
    (vec2 x y)))

(defclass mover ()
  ((location
    :initform (vec2 (/ *width* 2) 25)
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (radius
    :initform 25
    :accessor radius)))

(defmethod reverse-direction ((mover mover))
  (setf (velocity mover) (mult (velocity mover) -1)))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location))
         (r (radius mover))
         (top (+ y r))
         (right (+ x r))
         (bottom (- y r))
         (left (- x r))
         (pos (cond ((< left 0) (vec2 0 y))
                    ((> right *width*) (vec2 (- *width* r) y))
                    ((< bottom 0) (vec2 x r))
                    ((> top *height*) (vec2 x (- *height* r))))))
    (when pos (progn (reverse-direction mover)
                     (setf (location mover) pos)))))

(defmethod apply-force ((mover mover) force)
  (setf (acceleration mover) (add (acceleration mover) force)))

(defmethod update ((mover mover))
  (let* ((a (acceleration mover))
         (v (add (velocity mover) a))
         (v (limit-vec v 3)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))
    (setf (acceleration mover) (vec2 0 0)) ; Reset the acceleration.
    (check-edges mover)))

(defmethod display ((mover mover))
  (draw-circle (location mover) (radius mover)
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((mover
    :initform (make-instance 'mover)
    :accessor mover)
   (dt
    :initform 0
    :accessor dt))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Motion 101 (velocity and constant acceleration)"))

(defmethod draw ((this sketch))
  (display (mover this))
  (draw-text (format nil "~A" (velocity (mover this))) (vec2 0 0)))

(defmethod act ((this sketch))
  (let ((mover (mover this))
        (forces (list
                 (vec2 0 -0.1)           ; Gravity
                 (vec2 0 0.2)            ; Helium
                 (random-wind (dt this)) ; Wind
                 )))
    (dolist (force forces) (apply-force (mover this) force))
    (update mover)
    (incf (dt this) 0.01)))

(defun start-sketch ()
  (start 'sketch))
