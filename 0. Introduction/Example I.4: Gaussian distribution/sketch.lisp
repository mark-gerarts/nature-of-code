(defpackage :sketch (:use :cl :trivial-gamekit :alexandria)
            (:shadowing-import-from :alexandria :lerp))
(in-package :sketch)

(defvar *width* 640)
(defvar *height* 240)

(defvar *transparent-black* (vec4 0 0 0 0.1))

(defun gaussian-center-x ()
  (let ((r (gaussian-random))
        (sd 60)
        (mean (/ *width* 2)))
    (+ (* r sd) mean)))

(defclass dot ()
  ((pos
    :initform (vec2 (gaussian-center-x) (/ *height* 2))
    :accessor pos)
   (radius
    :initform 10
    :accessor radius)))

(defmethod draw ((dot dot))
  (draw-circle (pos dot) (radius dot)
               :fill-paint *transparent-black*))

(defgame sketch ()
  ((dot
    :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'dot)
    :accessor dots))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Gaussian distribution"))

(defmethod draw ((this sketch))
  (loop for dot across (dots this) do (draw dot)))

(defmethod act ((this sketch))
  (vector-push-extend (make-instance 'dot) (dots this)))

(start 'sketch)
