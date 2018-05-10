(defpackage :sketch (:use :cl :trivial-gamekit)
            (:import-from :cl-bodge :vector-length :normalize))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defvar *mouse-location* (vec2 0 0))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defclass mover ()
  ((location
    :initform (vec2 0 0)
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (top-speed
    :initform 10
    :accessor top-speed)))

(defmethod update ((mover mover))
  (let* ((a (subt *mouse-location* (location mover)))
         (a (div a 200))
         (v (add a (velocity mover)))
         (v (limit-vec v 10)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))))

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
  (:viewport-title "Acceleration towards the mouse"))

(defmethod post-initialize ((sketch sketch))
  (bind-cursor (lambda (x y) (setf *mouse-location* (vec2 x y)))))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update (mover this)))

(start 'sketch)
