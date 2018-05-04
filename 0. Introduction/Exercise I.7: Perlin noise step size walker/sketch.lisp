;; Note: might want to define an asdf system because dependencies are
;; getting numerous.
(defpackage :sketch (:use :cl :trivial-gamekit :black-tie))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun perlin-noise-1d (dt axis)
  "Perlin noise for the given axis. Ranges from -1 to 1."
  (let ((r (case axis
             (x (perlin-noise-ref dt 0.0f0 0.0f0))
             (y (perlin-noise-ref 0.0f0 dt 0.0f0))
             (z (perlin-noise-ref 0.0f0 0.0f0 dt)))))
    ;; *2 to make it range between -1 and 1.
    (* r 2)))

(defclass walker ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (previous-positions
    :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'vec2)
    :accessor previous-positions)
   (tx
    :initform 0
    :accessor tx)
   (ty
    :initform 0
    :accessor ty)))

(defmethod step-walker ((this walker))
  (let* ((rx (perlin-noise-1d (tx this) 'y))
         (ry (perlin-noise-1d (ty this) 'x))
         (current-x (x (pos this)))
         (current-y (y (pos this)))
         (new-pos (vec2 (+ rx current-x) (+ ry current-y))))
    (vector-push-extend (pos this) (previous-positions this))
    (setf (pos this) new-pos)
    (incf (tx this) 0.005)
    (incf (ty this) 0.005)))

(defmethod draw ((w walker))
  (loop for pos across (previous-positions w)
        do (draw-rect pos 1 1 :fill-paint *black*)))

(defgame sketch ()
  ((walker
    :initform (make-instance 'walker)
    :accessor walker))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Perlin noise walker"))

(defmethod draw ((this sketch))
  (draw (walker this)))

(defmethod act ((this sketch))
  (step-walker (walker this)))

(start 'sketch)
