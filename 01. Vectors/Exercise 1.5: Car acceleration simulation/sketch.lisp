(defpackage :sketch (:use :cl :trivial-gamekit)
            (:import-from :cl-bodge :vector-length :normalize))
(in-package :sketch)

(defvar *width* 640)
(defvar *height* 360)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 1))

;; ;; Define resources.
;; (register-resource-package
;;  :sketch
;;  ;; Gamekit expects an absolute path. Should this cause an error, make sure
;;  ;; the cwd is correct.
;;  *default-pathname-defaults*)

(define-image 'sketch::cars
  (merge-pathnames "car_sprite.png" *default-pathname-defaults*))

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defclass mover ()
  ((location
    :initform (vec2 (random-in-range 0 *width*) (random-in-range 0 *height*))
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 -0.001 0.01)
    :accessor acceleration)
   (top-speed
    :initform 10
    :accessor top-speed)))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location)))
    (cond ((< x 0) (setf (x location) *width*))
          ((> x *width*) (setf (x location) 0)))
    (cond ((< y 0) (setf (y location) *height*))
          ((> y *height*) (setf (y location) 0)))))

(defmethod update ((mover mover))
  (let* ((a (acceleration mover))
         (v (limit-vec (add (velocity mover) a) (top-speed mover))))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))
    (check-edges mover)))

(defmethod display ((mover mover))
  (draw-image (location mover) 'sketch::cars))

(defgame sketch ()
  ((mover
    :initform (make-instance 'mover)
    :accessor mover))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Car acceleration simulation"))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update (mover this)))

(start 'sketch)