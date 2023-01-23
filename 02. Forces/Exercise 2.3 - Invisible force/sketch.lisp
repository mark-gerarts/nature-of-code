(defpackage :nature-of-code.forces.exercise-3
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.exercise-3)

(defvar *width* 800)
(defvar *height* 200)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 0.4))

(defclass mover ()
  ((location
    :initform (vec2 50 (- *height* 2))
    :accessor location)
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (acceleration
    :initform (vec2 0 0)
    :accessor acceleration)
   (radius
    :accessor radius)
   (mass
    :initform (+ (random 5.0) 0.1)
    :accessor mass
    :initarg :mass)))

(defmethod initialize-instance :after ((mover mover) &key)
  ;; Make the radius dependent on the mass of the mover.
  (setf (radius  mover) (* 4 (mass mover))))

(defmethod reverse-direction ((mover mover) dir)
  (let ((vx (x (velocity mover)))
        (vy (y (velocity mover))))
    (if (eq dir 'x)
        (setf (x (velocity mover)) (- vx))
        (setf (y (velocity mover)) (- vy)))))

(defmethod check-edges ((mover mover))
  (let* ((location (location mover))
         (x (x location))
         (y (y location))
         (r (radius mover))
         (top (+ y r))
         (right (+ x r))
         (bottom (- y r))
         (left (- x r)))
    (cond ((< left 0) (progn
                        (setf (x (location mover)) r)
                        (reverse-direction mover 'x)))
          ((> right *width*) (progn
                               (setf (x (location mover)) (- *width* r))
                               (reverse-direction mover 'x)))
          ((< bottom 0) (progn
                          (setf (y (location mover)) r)
                          (reverse-direction mover 'y)))
          ((> top *height*) (progn
                              (setf (y (location mover)) (- *height* r))
                              (reverse-direction mover 'y))))))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

(defmethod apply-invisible-force ((mover mover))
  "This is what this exercise is all about. We create a force that in the
   direction of the center of the window. The strength of this forces is linear
   to the distance to the center."
  (let* ((location (location mover))
         (x (/ (x location) *width*))   ; % x
         (y (/ (y location) *height*))  ; % y
         ;; Since the % goes from 0 to 1, we can just subtract it from 0.5 to
         ;; get a force that points to the center.
         (Fx (- 0.5 x))
         (Fy (- 0.5 y)))
    (apply-force mover (vec2 Fx Fy))))

(defmethod update ((mover mover))
  (let* ((a (acceleration mover))
         (v (add (velocity mover) a)))
    (setf (velocity mover) v)
    (setf (location mover) (add v (location mover)))
    (setf (acceleration mover) (vec2 0 0)) ; Reset the acceleration.
    (check-edges mover)))

(defmethod display ((mover mover))
  (draw-circle (location mover) (radius mover)
               :fill-paint *gray*
               :stroke-paint *black*
               :thickness 2))

(defgame sketch ()
  ((movers
    :accessor movers))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Invisible force"))

(defmethod post-initialize ((this sketch))
  (setf (movers this)
        (make-array 50
                    :element-type 'mover
                    :initial-contents (loop for i upto 49
                                            collect (make-instance 'mover)))))

(defmethod draw ((this sketch))
  (loop for m across (movers this) do (display m)))

(defmethod act ((this sketch))
    (loop for mover across (movers this)
          do (progn
               (apply-invisible-force mover)
               (update mover))))

(defun start-sketch ()
  (start 'sketch))
