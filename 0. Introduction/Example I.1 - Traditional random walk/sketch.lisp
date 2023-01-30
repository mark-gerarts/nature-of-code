(defpackage :nature-of-code.introduction.example-1
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.introduction.example-1)

(defvar *width* 800)
(defvar *height* 600)
(defvar *black* (vec4 0 0 0 1))

(defun empty-array ()
  (make-array 1 :adjustable t :fill-pointer 0))

(defun mid ()
  (vec2 (/ *width* 2) (/ *height* 2)))

;; We'll represent a walker without using CLOS to minimize memory usage. This
;; because we have to remember previous states.
(defparameter *previous-states* (empty-array))
(defparameter *walker* (mid))

(defun draw-walker (walker)
  (draw-rect walker 1 1 :fill-paint *black*))

(defun step-walker (walker)
  (let* ((rx (1- (random 2.0)))
         (ry (1- (random 2.0)))
         (new-x (+ rx (x walker)))
         (new-y (+ ry (y walker)))
         (new-walker (vec2 new-x new-y)))
    ;; Store the old state and update the walker.
    (vector-push-extend *walker* *previous-states*)
    (setf *walker* new-walker)))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Random walker"))

(defmethod post-initialize ((this sketch))
  (setf *walker* (mid))
  (setf *previous-states* (empty-array)))

(defmethod draw ((this sketch))
  (loop for w across *previous-states* do (draw-walker w)))

(defmethod act ((this sketch))
  (step-walker *walker*))

(defun start-sketch ()
  (start 'sketch))
