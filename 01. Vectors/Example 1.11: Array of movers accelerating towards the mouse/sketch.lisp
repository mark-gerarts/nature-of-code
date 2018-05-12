(defpackage :nature-of-code.vectors.example-11
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.vectors.example-11)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defvar *mouse-location* (vec2 0 0))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

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

(defmethod update ((mover mover))
  (let* ((a (subt *mouse-location* (location mover)))
         (a (normalize a))
         (a (mult a 0.5))
         (v (add a (velocity mover)))
         (v (limit-vec v 5)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))))

(defmethod display ((mover mover))
  (draw-circle (location mover) 16
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((movers
    :initform (let* ((n 20)
                     (movers (loop for i to (1- n)
                                   collect (make-instance 'mover))))
                (make-array n :initial-contents movers))
    :accessor movers))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Acceleration towards the mouse"))

(defmethod post-initialize ((sketch sketch))
  (bind-cursor (lambda (x y) (setf *mouse-location* (vec2 x y)))))

(defmethod draw ((this sketch))
  (loop for m across (movers this) do (display m)))

(defmethod act ((this sketch))
  (loop for m across (movers this) do (update m)))

(defun start-sketch ()
  (start 'sketch))
