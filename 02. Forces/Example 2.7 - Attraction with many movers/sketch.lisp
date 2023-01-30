(defpackage :nature-of-code.forces.example-7
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.example-7)

(defvar *width* 600)
(defvar *height* 400)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 0.4))

(defun constrain (value min max)
  (if (> value max)
      max
      (if (< value min)
          min
          value)))

(defclass mover ()
  ((location
    :accessor location
    :initarg :location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 1 0)
    :accessor acceleration)
   (radius
    :accessor radius)
   (mass
    :initform (+ (random 5) 1)
    :accessor mass
    :initarg :mass)))

(defmethod initialize-instance :after ((mover mover) &key)
  ;; Make the radius dependent on the mass of the mover.
  (setf (radius  mover) (* 4 (mass mover)))
  (setf (location mover) (vec2 (random *width*) (random *height*))))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

(defmethod update ((mover mover))
  (let* ((a (acceleration mover))
         (v (add (velocity mover) a)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))
    (setf (acceleration mover) (vec2 0 0))))

(defmethod display ((mover mover))
  (draw-circle (location mover) (radius mover)
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defclass attractor ()
  ((location
    :accessor location
    :initform (vec2 (/ *width* 2) (/ *height* 2)))
   (mass
    :accessor mass
    :initform 20)
   (g
    :accessor g
    :initform 0.4)))

(defmethod display ((attractor attractor))
  (draw-circle (location attractor) (* 2 (mass attractor))
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defmethod attract ((attractor attractor) (mover mover))
  (let* ((force (subt (location attractor) (location mover)))
         ;; Constrain the distance to prevent large values when the mover gets
         ;; really close.
         (distance (constrain (vector-length force) 5 25))
         ;; F = G (m1 * m2) / r^2
         (strength (/
                    (* (g attractor) (mass attractor) (mass mover))
                    (expt distance 2))))
    (mult (normalize force) strength)))

(defgame sketch ()
  ((mover
    :accessor movers
    :initform (loop repeat 10 collect (make-instance 'mover)))
   (attractor
    :accessor attractor
    :initform (make-instance 'attractor)))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Attraction with many movers"))

(defmethod post-initialize ((this sketch)))

(defmethod draw ((this sketch))
  (mapc #'display (movers this))
  (display (attractor this)))

(defmethod act ((this sketch))
  (with-accessors ((movers movers) (attractor attractor)) this
    (dolist (mover movers)
      (progn
        (apply-force mover (attract attractor mover))
        (update mover)))))

(defun start-sketch ()
  (start 'sketch))
