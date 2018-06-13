(defpackage :nature-of-code.fractals.example-8.7
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge))
(in-package :nature-of-code.fractals.example-8.7)

(defvar *width* 600)
(defvar *height* 400)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defvar *mouse-position* (vec2 *center-x* *center-y*))

(defgame sketch () ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Dynamic recursive tree"))

(defmethod post-initialize ((this sketch))
  (bind-cursor (lambda (x y) (setf *mouse-position* (vec2 x y)))))

(defun draw-black-line (origin end)
  (draw-line origin end *black* :thickness 3))

(defun branch (len &optional (theta (/ pi 6)))
  (draw-black-line (vec2 0 0) (vec2 0 len))
  (translate-canvas 0 len)

  (setf len (* 0.66 len))
  (when (> len 2)
    (with-pushed-canvas ()
      (rotate-canvas (- theta))
      (branch len theta))

    (with-pushed-canvas ()
      (rotate-canvas theta)
      (branch len theta))))

(defmethod draw ((this sketch))
  (translate-canvas *center-x* 0)
  (let* ((mouse-x (x *mouse-position*))
         ;; Map the mouse position to a value between 0 and PI/2.
         (theta (* mouse-x (/ (/ pi 2) *width*))))
    (branch 100 theta)))


(defmethod act ((this sketch)))

(defun start-sketch ()
  (start 'sketch))
