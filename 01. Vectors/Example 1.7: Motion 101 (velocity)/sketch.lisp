(defpackage :sketch (:use :cl :trivial-gamekit))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defclass mover ()
  ((location
    :initform (vec2 (random *width*) (random *height*))
    :accessor location)
   (velocity
    :initform (vec2 (random-in-range -2 2) (random-in-range -2 2))
    :accessor velocity)))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location)))
    (cond ((< x 0) (setf (x location) *width*))
          ((> x *width*) (setf (x location) 0)))
    (cond ((< y 0) (setf (y location) *height*))
          ((> y *height*) (setf (y location) 0)))))

(defmethod update ((mover mover))
  (setf (location mover) (add (location mover) (velocity mover)))
  (check-edges mover))

(defmethod display ((mover mover))
  (draw-circle (location mover) 16
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((mover
    :initform (make-instance 'mover)
    :accessor mover))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Motion 101 (velocity)"))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update (mover this)))

(start 'sketch)
