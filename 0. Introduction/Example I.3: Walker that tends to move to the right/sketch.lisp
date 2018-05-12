(defpackage :nature-of-code.introduction.example-3
  (:export :start-sketch)
  (:use :cl :trivial-gamekit))
(in-package :nature-of-code.introduction.example-3)

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
  (let ((r (random 1.0))
        (x (x *walker*))
        (y (y *walker*)))
    ;; Store the old state and update the walker.
    (vector-push-extend *walker* *previous-states*)
    (cond
      ((< r 0.4) (incf x))
      ((< r 0.6) (decf x))
      ((< r 0.8) (incf y))
      (t (decf y)))
    (setf *walker* (vec2 x y))))

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
