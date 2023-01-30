(defpackage :nature-of-code.fractals.exercise-8.9
  (:export :start-sketch)
  (:use :cl :trivial-gamekit)
  (:import-from :alexandria :random-elt))
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

;;; This is a remnant of a previous exercise, where it made sense to share some
;;; properties between leaves and branches.
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
    :accessor color)
   (radius
    :initarg :radius
    :accessor radius)))

;;; See above. Leaves are put in the branches array as well. Might be a good
;;; idea to refactor this.
(defmethod grow ((leaf leaf) dt))
(defmethod fully-grown-p ((leaf leaf)) t)
(defmethod can-have-leaf-p ((leaf leaf)) nil)

(defmethod display ((leaf leaf))
  (with-pushed-canvas ()
    (draw-circle (origin leaf) (radius leaf) :fill-paint (color leaf))))

(defclass branch (tree-part)
  ((branch-length
    :initform 0
    :accessor branch-length)
   (max-length
    :initarg :max-length
    :accessor max-length)
   (splitp
    :initform 'nil
    :accessor splitp
    :documentation "Indicates if this branch has been split.")
   (has-leaf-p
    :initform 'nil
    :accessor has-leaf-p)))

(defmethod end ((branch branch))
  (with-slots (origin branch-length direction) branch
    (add (mult direction branch-length) origin)))

(defmethod grow ((branch branch) dt)
  (with-accessors ((current branch-length) (max max-length)) branch
    (incf current dt)
    (when (> current max) (setf current max))))

(defmethod fully-grown-p ((branch branch))
  (>= (branch-length branch) (max-length branch)))

(defmethod can-have-leaf-p ((branch branch))
  (and (not (splitp branch)) (not (has-leaf-p branch))))

(defmethod split-branch ((branch branch) &key (direction :left))
  (let ((theta (if (equalp direction :left) *theta* (- *theta*))))
    (setf (splitp branch) t)
    (make-instance 'branch
                   :origin (end branch)
                   :max-length (* *length-reduction* (max-length branch))
                   :direction (rotate (direction branch) theta))))

(defmethod display ((branch branch))
  (with-accessors ((origin origin) (end end)) branch
    (draw-line origin end *black* :thickness 3)))

(defun split (branches)
  (loop for branch across branches
        unless (splitp branch)
          do (progn
               (vector-push-extend
                (split-branch branch :direction :left) branches)
               (vector-push-extend
                (split-branch branch :direction :right) branches)))
  branches)

(defun add-random-leaf (branches)
  (let ((endpoints (remove-if-not #'can-have-leaf-p branches)))
    (unless (= 0 (length endpoints))
      (let ((branch (random-elt endpoints)))
        (vector-push-extend
         (make-instance 'leaf
                        :color (vec4 0.2 0.6 0.2 0.8)
                        :radius (+ 5 (random 10))
                        :origin (end branch)
                        :direction (direction branch))
         branches)
        (setf (has-leaf-p branch) t)))))

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

(defun animate-growth (branches dt)
  (loop for branch across branches
        do (grow branch dt))
  (when (every #'fully-grown-p branches)
    (if (< (length branches) 128)
        (split branches)
        (add-random-leaf branches))))

(defgame sketch ()
  ((branches
    :initform (make-array 0 :adjustable t :fill-pointer 0)
    :accessor branches)
   (dt
    :initform 0
    :accessor dt))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "List tree"))

(defmethod increment-time ((this sketch))
  (incf (dt this) 0.05))

(defmethod post-initialize ((this sketch))
  (with-accessors ((branches branches)) this
    (vector-push-extend
     (make-instance 'branch :origin (vec2 300 0)
                            :max-length 100
                            :direction (vec2 0 1))
     branches)))

(defmethod draw ((this sketch))
  (loop for branch across (branches this) do (display branch)))

(defmethod act ((this sketch))
  (increment-time this)
  (animate-growth (branches this) (dt this)))

(defun start-sketch ()
  (start 'sketch))
