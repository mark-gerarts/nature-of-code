;; See readme notes for running this.
(defpackage :vectors-car-acceleration (:use :cl :trivial-gamekit)
            (:import-from :cl-bodge :vector-length :normalize)
            (:export :start-sketch))
(in-package :vectors-car-acceleration)

(defvar *width* 800)
(defvar *height* 240)

;; Register resources
(register-resource-package
 :keyword
 (asdf:system-relative-pathname :vectors-car-acceleration "assets/"))

(define-image :sprite "sprite.png")

(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defclass mover ()
  ((location
    :initform (vec2 0 0)
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (top-speed
    :initform 10
    :accessor top-speed)
   (acceleration-delta
    :initform 0.01
    :reader da)))

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
  (draw-image (location mover) :sprite))

(defmethod accelerate ((mover mover))
  (incf (x (acceleration mover)) (da mover)))

(defmethod decelerate ((mover mover))
  (decf (x (acceleration mover)) (da mover)))

(defgame sketch ()
  ((mover
    :initform (make-instance 'mover)
    :accessor mover))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Car acceleration simulation"))

(defmethod post-initialize ((this sketch))
  (bind-button :up :pressed (lambda () (accelerate (mover this))))
  (bind-button :down :pressed (lambda () (decelerate (mover this)))))

(defmethod draw ((this sketch))
  (display (mover this)))

(defmethod act ((this sketch))
  (update (mover this)))

(defun start-sketch ()
  (start 'sketch))
