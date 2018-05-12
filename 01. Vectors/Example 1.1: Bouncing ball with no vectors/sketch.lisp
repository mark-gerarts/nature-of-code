(defpackage :nature-of-code.vectors.example-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:shadow :step))
(in-package :nature-of-code.vectors.example-1)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defclass ball ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (dx
    :initform 2
    :accessor dx)
   (dy
    :initform 3.3
    :accessor dy)))

(defmethod step ((ball ball))
  (let* ((x (x (pos ball)))
         (y (y (pos ball)))
         (new-x (+ x (dx ball)))
         (new-y (+ y (dy ball))))
    (setf (pos ball) (vec2 new-x new-y))      ; Update position
    (when (or (< new-x 0) (> new-x *width*))  ; Reverse x direction if needed
      (setf (dx ball) (- (dx ball))))
    (when (or (< new-y 0) (> new-y *height*)) ; Reverse y direction if needed
      (setf (dy ball) (- (dy ball))))))

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

(defun start-sketch ()
  (start 'sketch))
