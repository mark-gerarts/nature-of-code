(defpackage :sketch (:use :cl :trivial-gamekit))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)
(defvar *black* (vec4 0 0 0 1))
(defvar *mouse-position* (vec2 0 0))

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

(defun random-step (pos)
  "Step into a random direction."
  (let ((rx (1- (random 2.0)))
        (ry (1- (random 2.0))))
    (vec2 (+ rx (x pos)) (+ ry (y pos)))))

(defun move-to-point (pos point)
  "Step in the direction of the given point."
  (let* ((diff (subt point pos))
         (length (abs (sqrt (+ (expt (x diff) 2) (expt (y diff) 2))))))
    (add (div diff length) pos)))

(defmethod step-walker ((w walker))
  (let ((new-pos (if (= (random 2) 1)
                     (move-to-point (pos w) *mouse-position*) ; Towards mouse
                     (random-step (pos w)))))                 ; Random
    (vector-push-extend (pos w) (previous-positions w))
    (setf (pos w) new-pos)))

(defgame sketch ()
  ((walker
    :initform (make-instance 'walker)
    :accessor walker))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Dynamic random walker"))

(defmethod post-initialize ((this sketch))
  (bind-cursor (lambda (x y)
                 (setf *mouse-position* (vec2 x y)))))

(defmethod draw ((this sketch))
  (draw-text (format nil "~A" *mouse-position*) (vec2 0 0 ))
  (draw (walker this)))

(defmethod act ((this sketch))
  (step-walker (walker this)))

(start 'sketch)
