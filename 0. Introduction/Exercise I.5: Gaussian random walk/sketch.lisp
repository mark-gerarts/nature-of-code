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

(defun gaussian-random-step (pos)
  "Step into a random direction."
  (let ((rx (1- (gaussian-random-with 0.1 1)))
        (ry (1- (gaussian-random-with 0.1 1))))
    (vec2 (+ rx (x pos)) (+ ry (y pos)))))

(defmethod step-walker ((w walker))
  (let ((new-pos (gaussian-random-step (pos w))))
    (vector-push-extend (pos w) (previous-positions w))
    (setf (pos w) new-pos)))

(defgame sketch ()
  ((walker
    :initform (make-instance 'walker)
    :accessor walker))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Dynamic random walker"))

(defmethod draw ((this sketch))
  (draw (walker this)))

(defmethod act ((this sketch))
  (step-walker (walker this)))

(start 'sketch)
