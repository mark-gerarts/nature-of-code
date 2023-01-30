(defpackage :nature-of-code.vectors.example-5
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge.engine :vector-length))
(in-package :nature-of-code.vectors.example-5)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))

(defun draw-magnitude (magnitude)
  (let ((rect-height 10))
    (draw-rect (vec2 0 (- *height* rect-height))
               magnitude
               rect-height
               :fill-paint *black*)))

(defgame sketch ()
  ((center
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor center)
   (mouse-position
    :initform (vec2 0 0)
    :accessor mouse-position))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Vector magnitude"))

(defmethod post-initialize ((this sketch))
  (bind-cursor (lambda (x y)
                 (setf (mouse-position this) (vec2 x y)))))

(defmethod draw ((this sketch))
  (let* ((center (center this))
         (sub (subt (mouse-position this) center)))
    (with-pushed-canvas ()
      (translate-canvas (x center) (y center))
      (draw-line (vec2 0 0) sub *black* :thickness 2)) ; The vector
    (draw-magnitude (vector-length sub))))             ; The magnitude

(defun start-sketch ()
  (start 'sketch))
