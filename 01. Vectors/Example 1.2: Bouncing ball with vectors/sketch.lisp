(defpackage :sketch (:use :cl :trivial-gamekit)
            (:shadow :step))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defclass ball ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (velocity
    :initform (vec2 2 3.3)
    :accessor velocity)))

(defmethod step ((ball ball))
  (setf (pos ball) (add (pos ball) (velocity ball)))
  (let* ((x (x (pos ball)))
         (y (y (pos ball)))
         (v (velocity ball)))
    (when (or (< x 0) (> x *width*))
      (setf (velocity ball) (vec2 (- (x v)) (y v))))
    (when (or (< y 0) (> y *height*))
      (setf (velocity ball) (vec2 (x v) (- (y v)))))))

(defmethod draw ((ball ball))
  (draw-circle (pos ball) 25
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((ball
    :initform (make-instance 'ball)
    :accessor ball))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Bouncing ball without vectors"))

(defmethod draw ((this sketch))
  (draw (ball this)))

(defmethod act ((this sketch))
  (step (ball this)))

(start 'sketch)
