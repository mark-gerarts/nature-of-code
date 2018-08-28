(defpackage :nature-of-code.forces.example-6
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.example-6)

(defvar *width* 600)
(defvar *height* 400)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 0.4))

(defclass mover ()
  ((location
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (radius
    :accessor radius)
   (mass
    :initform (+ (random 5.0) 1.0)
    :accessor mass
    :initarg :mass)))

(defmethod initialize-instance :after ((mover mover) &key)
  ;; Make the radius dependent on the mass of the mover.
  (setf (radius  mover) (* 4 (mass mover)))
  ;; Spawn somewhere random, but at maximum height.
  (setf (location mover) (vec2 (random *width*) (- *height* (radius mover)))))

(defmethod reverse-direction ((mover mover) dir)
  (let ((vx (x (velocity mover)))
        (vy (y (velocity mover))))
    (if (eq dir 'x)
        (setf (x (velocity mover)) (- vx))
        (setf (y (velocity mover)) (- vy)))))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location))
         (r (radius mover))
         (top (+ y r))
         (right (+ x r))
         (bottom (- y r))
         (left (- x r)))
    (when (< left 0)
      (progn
        (setf (x (location mover)) r)
        (reverse-direction mover 'x)))
    (when (> right *width*)
      (progn
        (setf (x (location mover)) (- *width* r))
        (reverse-direction mover 'x)))
    (when (< bottom 0)
      (progn
        (setf (y (location mover)) r)
        (reverse-direction mover 'y)))
    (when (> top *height*)
      (progn
        (setf (y (location mover)) (- *height* r))
        (reverse-direction mover 'y)))))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

(defmethod update ((mover mover))
  (let* ((a (acceleration mover))
         (v (add (velocity mover) a)))
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
    :accessor mover
    :initform (make-instance 'mover)))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Attraction"))

(defmethod post-initialize ((this sketch)))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update mover))

(defun start-sketch ()
  (start 'sketch))
