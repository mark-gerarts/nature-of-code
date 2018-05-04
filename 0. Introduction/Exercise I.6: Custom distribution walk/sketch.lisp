(defpackage :sketch (:use :cl :trivial-gamekit :alexandria)
            (:shadowing-import-from :alexandria :lerp))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)
(defvar *black* (vec4 0 0 0 1))

(defun gaussian-random-with (sd mean)
  "Helper function to get a random gaussian value with the given sd and mean."
  (+ (* sd (gaussian-random)) mean))

(defclass walker ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (previous-positions
    :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'vec2)
    :accessor previous-positions)))

(defmethod draw ((w walker))
  (loop for pos across (previous-positions w)
        do (draw-rect pos 1 1 :fill-paint *black*)))

(defun expt-random ()
  (let* ((r1 (random 10))
         (r2 (random 10))
         (p (expt r1 2)))
    (if (< r2 p) r1 (expt-random))))

(defun random-in-range (min max)
  (+ min (random (+ 1 (- max min)))))

(defun custom-random-step (pos)
  (let* ((stepsize (expt-random))
         (dx (random-in-range (- stepsize) stepsize))
         (dy (random-in-range (- stepsize) stepsize)))
    (vec2 (+ dx (x pos)) (+ dy (y pos)))))

(defmethod step-walker ((w walker))
  (let ((new-pos (custom-random-step (pos w))))
    (vector-push-extend (pos w) (previous-positions w))
    (setf (pos w) new-pos)))

(defgame sketch ()
  ((walker
    :initform (make-instance 'walker)
    :accessor walker))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Custom distribution walk"))

(defmethod draw ((this sketch))
  (draw (walker this)))

(defmethod act ((this sketch))
  (step-walker (walker this)))

(start 'sketch)
