(defpackage :nature-of-code.fractals.exercise-8.9
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge))
(in-package :nature-of-code.fractals.exercise-8.9)

(defvar *width* 600)
(defvar *height* 400)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

(defvar *black* (vec4 0 0 0 1))

(defvar *theta* (/ pi 6))
(defvar *length-reduction* 0.66)

(defun rotate (vector theta)
  "Rotates a (unit) vector."
  (with-accessors ((x x) (y y)) vector
    (vec2
     (- (* x (cos theta)) (* y (sin theta)))
     (+ (* x (sin theta)) (* y (cos theta))))))

(defclass tree-part ()
  ((origin
    :initarg :origin
    :accessor origin)
   (direction
    :initarg :direction
    :accessor direction
    :documentation "Unit vector indicating the direction.")))

(defclass leaf (tree-part)
  ((color
    :initarg :color
    :accessor color)))

(defmethod display ((leaf leaf))
  (with-pushed-canvas ()
      (rotate-canvas (atan (y (direction leaf)) (x (direction leaf))))
      (draw-ellipse (origin leaf) 7 20 :fill-paint (color leaf))))

(defclass branch (tree-part)
  ((end
    :accessor end)
   (branch-length
    :initarg :length
    :accessor branch-length)
   (splitp
    :initform 'nil
    :accessor splitp
    :documentation "Indicates if this branch has been split.")))

(defmethod initialize-instance :after ((branch branch) &key)
  ;; We'll calculate the end after initializing the branch.
  (with-slots (origin end branch-length direction) branch
    (setf end (add (mult direction branch-length) origin))))

(defmethod split-branch ((branch branch) &key (direction :left))
  (let ((theta (if (equalp direction :left) *theta* (- *theta*))))
    (setf (splitp branch) t)
    (make-instance 'branch
                   :origin (end branch)
                   :length (* *length-reduction* (branch-length branch))
                   :direction (rotate (direction branch) theta))))

(defmethod display ((branch branch))
  (with-accessors ((origin origin) (end end)) branch
    (draw-line origin end *black* :thickness 3)))

(defgame sketch ()
  ((branches
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor branches))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "List tree"))

(defun split (branches)
  (loop for branch across branches
        unless (splitp branch)
          do (progn
               (vector-push-extend
                (split-branch branch :direction :left) branches)
               (vector-push-extend
                (split-branch branch :direction :right) branches)))
  branches)

(defun add-leaves (branches)
  (loop for branch across branches
        unless (splitp branch)
          do (vector-push-extend
              (make-instance 'leaf
                             :color (vec4 0.2 0.6 0.2 1)
                             :origin (end branch)
                             :direction (direction branch))
              branches)))

(defun branch (len &optional (theta (/ pi 6)) (thickness 8))
  (draw-line (vec2 0 0) (vec2 0 len) *black* :thickness thickness)
  (translate-canvas 0 len)

  (setf len (* 0.66 len))
  (setf thickness (* 0.8 thickness))

  (when (and (> len 2) (> thickness 0))
    (with-pushed-canvas ()
      (rotate-canvas (- theta))
      (branch len theta thickness))

    (with-pushed-canvas ()
      (rotate-canvas theta)
      (branch len theta thickness))))

(defmethod post-initialize ((this sketch))
  (with-accessors ((branches branches)) this
    (vector-push-extend
     (make-instance 'branch :origin (vec2 300 0) :length 100 :direction (vec2 0 1))
     branches)
    (loop repeat 7 do (setf branches (split branches)))
    (add-leaves branches)))

(defmethod draw ((this sketch))
  (loop for branch across (branches this) do (display branch)))

(defun start-sketch ()
  (start 'sketch))
