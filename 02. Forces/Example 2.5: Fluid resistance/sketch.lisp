(defpackage :nature-of-code.forces.example-5
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.forces.example-5)

(defvar *width* 800)
(defvar *height* 200)

(defvar *black* (vec4 0 0 0 1))
(defvar *gray* (vec4 0.5 0.5 0.5 0.4))
(defvar *liquid* (vec4 0.2 0.2 0.2 0.8))

(defclass liquid ()
  ((drag-coefficient
    :initarg :drag-coefficient
    :initform 0.2
    :accessor drag-coefficent)
   (width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)
   (origin
    :initarg :origin
    :initform (vec2 0 0)
    :accessor origin)))

(defmethod display ((liquid liquid))
  (draw-rect (origin liquid)
             (width liquid)
             (height liquid)
             :fill-paint *liquid*))

(defclass mover ()
  ((location
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
    :initform (+ (random 5.0) 1.0)
    :accessor mass
    :initarg :mass)))

(defmethod initialize-instance :after ((mover mover) &key)
  ;; Make the radius dependent on the mass of the mover.
  (setf (radius  mover) (* 4 (mass mover)))
  ;; Spawn somewhere random, but at maximum height.
  (setf (location mover) (vec2 (random *width*) (- *height* (radius mover)))))

(defmethod is-inside ((mover mover) (liquid liquid))
  (let ((x (x (location mover)))
        (y (y (location mover)))
        (lx (x (origin liquid)))
        (ly (y (origin liquid)))
        (w (width liquid))
        (h (height liquid)))
    ;; Some standard AABB.
    (and (> x lx) (< x (+ lx w)) (> y ly) (< y (+ ly h)))))

(defmethod drag ((mover mover) (liquid liquid))
  (let* ((v (velocity mover))
         (v-magn (vector-length v))
         (drag-magn (* (drag-coefficent liquid)  v-magn v-magn))
         (drag v)
         (drag (mult drag -1))
         (drag (normalize drag))
         (drag (mult drag drag-magn)))
    (apply-force mover drag)))

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
    (when (< left 0)
      (progn
        (setf (x (location mover)) r)
        (reverse-direction mover 'x)))
    (when (> right *width*)
      (progn
        (setf (x (location mover)) (- *width* r))
        (reverse-direction mover 'x)))
    (when (< bottom 0)
      (progn
        (setf (y (location mover)) r)
        (reverse-direction mover 'y)))
    (when (> top *height*)
      (progn
        (setf (y (location mover)) (- *height* r))
        (reverse-direction mover 'y)))))

(defmethod apply-force ((mover mover) force)
  (let ((f (div force (mass mover))))
    (setf (acceleration mover) (add (acceleration mover) f))))

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
    :accessor movers)
   (liquid
    :initform (make-instance 'liquid :width *width* :height (/ *height* 2))
    :accessor liquid))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Fluid resistance"))

(defmethod post-initialize ((this sketch))
  (setf (movers this)
        (make-array 10
                    :element-type 'mover
                    :initial-contents (loop repeat 10
                                            collect (make-instance 'mover)))))

(defmethod draw ((this sketch))
  (loop for m across (movers this) do (display m))
  (display (liquid this)))

(defmethod act ((this sketch))
  (loop for mover across (movers this)
        do (progn (apply-force mover (mult (vec2 0 -0.1) (mass mover)))
                  (when (is-inside mover (liquid this))
                    (drag mover (liquid this)))
                  (update mover))))

(defun start-sketch ()
  (start 'sketch))
