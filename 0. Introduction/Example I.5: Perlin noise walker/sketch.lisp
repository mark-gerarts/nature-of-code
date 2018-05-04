
(defpackage :sketch (:use :cl :trivial-gamekit :black-tie))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

(defun perlin-noise-1d (dt coord)
  (case coord
    ('x ()))

(defclass walker ()
  ((pos
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor pos)
   (tx
    :initform 0
    :accessor tx)
   (ty
    :initform 0
    :accessor ty)))

(defmethod step-walker ((this walker))
  )

(defgame sketch ()
  ()
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Gaussian distribution"))

(defmethod draw ((this sketch))
  )

(defmethod act ((this sketch))
  )

(start 'sketch)
