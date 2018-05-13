(defpackage :nature-of-code.forces.example-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.example-1)

(defvar *width* 800)
(defvar *height* 200)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defclass mover ()
  ((location
    :initform (vec2 50 (- *height* 2))
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (radius
    :initform 25
    :accessor radius)
   (mass
    :initform 10
    :accessor mass)))

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
    (cond ((< left 0) (progn
                        (setf (x (location mover)) r)
                        (reverse-direction mover 'x)))
          ((> right *width*) (progn
                               (setf (x (location mover)) (- *width* r))
                               (reverse-direction mover 'x)))
          ((< bottom 0) (progn
                          (setf (y (location mover)) r)
                          (reverse-direction mover 'y)))
          ((> top *height*) (progn
                              (setf (y (location mover)) (- *height* r))
                              (reverse-direction mover 'y))))))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

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
    :accessor mover))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Forces"))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (let ((mover (mover this))
        (forces (list
                 (vec2 0 -0.2)          ; Gravity
                 (vec2 0.05 0)          ; Wind
                 )))
    (dolist (force forces) (apply-force (mover this) force))
    (update mover)))

(defun start-sketch ()
  (start 'sketch))
