(defpackage :nature-of-code.oscillation.example-2
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.oscillation.example-2)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 0.7))

(defun constrain (value min max)
  (cond
    ((> value max) max)
    ((< value min) min)
    ('t value)))

(defclass mover ()
  ((location
    :initarg :location
    :accessor location)
   (velocity
    :initform (vec2 (- (random 1.0) 1.0) (- (random 1.0) 1.0))
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (mass
    :initarg :mass
    :initform 15
    :accessor mass)
   (angle
    :initform 0
    :accessor angle)
   (a-velocity
    :initform 0
    :accessor a-velocity)
   (a-acceleration
    :initform 0
    :accessor a-acceleration)))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

(defmethod update ((mover mover))
  (setf (velocity mover) (add (velocity mover) (acceleration mover)))
  (setf (location mover) (add (location mover) (velocity mover)))
  (setf (a-acceleration mover) (/ (x (acceleration mover)) 10))
  (setf (a-velocity mover) (constrain (a-velocity mover) -0.1 0.1))
  (setf (a-velocity mover) (+ (a-velocity mover) (a-acceleration mover)))
  (setf (angle mover) (+ (angle mover) (a-velocity mover)))
  (setf (acceleration mover) (vec2 0 0)))

(defmethod display ((mover mover))
  (with-pushed-canvas ()
    (translate-canvas (x (location mover)) (y (location mover)))
    (rotate-canvas (angle mover))
    (draw-rect (vec2 0 0) (mass mover) (mass mover)
               :fill-paint *gray*
               :stroke-paint *black*)))

(defclass attractor ()
  ((location
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor location)
   (mass
    :initarg :mass
    :initform 20
    :accessor mass)
   (g
    :initarg :g
    :initform 0.4
    :accessor g)))

(defmethod display ((attractor attractor))
  (draw-circle
   (location attractor) (mass attractor)
   :fill-paint *gray* :stroke-paint *black* :thickness 2.0))

(defmethod attract ((attractor attractor) (mover mover))
  (let* ((force (subt (location attractor) (location mover)))
         (distance (constrain (vector-length force) 5 25))
         (strength (/
                    (* (g attractor) (mass attractor) (mass mover))
                    (expt distance 2))))
    (mult (normalize force) strength)))

(defun random-mover ()
  (make-instance
   'mover
   :location (vec2 (random *width*) (random *height*))
   :mass (+ (random 40) 15)))

(defgame sketch ()
  ((movers
    :initform '()
    :accessor movers)
   (attractor
    :accessor attractor))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Forces with (arbitrary) angular motion"))

(defmethod post-initialize ((this sketch))
  (setf (movers this)
        (loop repeat 10 collect (random-mover)))
  (setf (attractor this)
        (make-instance 'attractor)))

(defmethod draw ((this sketch))
  (loop for mover in (movers this) do (display mover))
  (display (attractor this)))

(defmethod act ((this sketch))
  (loop for mover in (movers this)
        do (progn
             (apply-force mover (attract (attractor this) mover))
             (update mover))))

(defun start-sketch ()
  (start 'sketch))
