(defpackage :sketch (:use :cl :trivial-gamekit)
            (:import-from :cl-bodge.math :vector-length :normalize))
(in-package :sketch)

(defvar *width* 800)
(defvar *height* 600)

(defvar *black* (vec4 0 0 0 1))

(defgame sketch ()
  ((center
    :initform (vec2 (/ *width* 2) (/ *height* 2))
    :accessor center)
   (mouse-position
    :initform (vec2 0 0)
    :accessor mouse-position))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Normalizing a vector"))

(defmethod post-initialize ((this sketch))
  (bind-cursor (lambda (x y)
                 (setf (mouse-position this) (vec2 x y)))))

(defmethod draw ((this sketch))
  (let* ((center (center this))
         (sub (subt (mouse-position this) center))
         (norm (* 50 (normalize sub))))
    (draw-text (format nil "~A" center) (vec2 0 0))))
    ;; (translate-canvas (x center) (y center))
    ;; (print norm)
    ;; (draw-line (vec2 0 0) norm *black* :thickness 2))

(start 'sketch)
