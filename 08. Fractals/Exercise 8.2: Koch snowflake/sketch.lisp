(defpackage :nature-of-code.fractals.exercise-8.2
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.fractals.exercise-8.2)

(defvar *width* 800)
(defvar *height* 800)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defun rotate (vector theta)
  (let* ((cos (cos theta))
         (sin (sin theta))
         (x1 (x vector))
         (y1 (y vector))
         (x2 (- (* cos x1) (* sin y1)))
         (y2 (+ (* sin x1) (* cos y1))))
    (vec2 x2 y2)))

(defun empty-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defclass koch-line ()
  ((origin
    :initarg :origin
    :accessor origin)
   (end
    :initarg :end
    :accessor end)))

(defun make-koch-line (origin end)
  (make-instance 'koch-line :origin origin :end end))

(defmethod display ((line koch-line))
  (draw-line (origin line) (end line) *black*))

(defmethod koch-a ((line koch-line))
  (origin line))

(defmethod koch-b ((line koch-line))
  (let* ((v (subt (end line) (origin line)))
         (v (div v 3))
         (v (add v (origin line))))
    v))

(defmethod koch-c ((line koch-line))
  (let* ((v (subt (end line) (origin line)))
         (v (div v 3))
         (a (add v (origin line)))
         (v (rotate v (/ pi 3)))
         (a (add a v)))
    a))

(defmethod koch-d ((line koch-line))
  (let* ((v (subt (end line) (origin line)))
         (v (mult v (/ 2 3)))
         (v (add v (origin line))))
    v))

(defmethod koch-e ((line koch-line))
  (end line))

(defmethod split ((line koch-line))
  (let ((a (koch-a line))
        (b (koch-b line))
        (c (koch-c line))
        (d (koch-d line))
        (e (koch-e line))
        (new-lines (empty-array)))
    (vector-push-extend (make-koch-line a b) new-lines)
    (vector-push-extend (make-koch-line b c) new-lines)
    (vector-push-extend (make-koch-line c d) new-lines)
    (vector-push-extend (make-koch-line d e) new-lines)
    new-lines))

(defun generate (lines)
  (let ((new-lines (empty-array)))
    (loop for line across lines do
      (loop for new-line across (split line) do
        (vector-push-extend new-line new-lines)))
    new-lines))

(defgame sketch ()
  ((lines
    :initform (empty-array)
    :accessor lines))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Koch snowflake"))

(defmethod post-initialize ((this sketch))
  (let* ((top (vec2 400 790))
         (bottom-right (vec2 700 190))
         (bottom-left (vec2 100 190)))
    (vector-push-extend (make-koch-line top bottom-right) (lines this))
    (vector-push-extend (make-koch-line bottom-right bottom-left) (lines this))
    (vector-push-extend (make-koch-line bottom-left top) (lines this))
    (loop repeat 5 do (setf (lines this) (generate (lines this))))))

(defmethod draw ((this sketch))
  (loop for line across (lines this) do (display line)))

(defun start-sketch ()
  (start 'sketch))
