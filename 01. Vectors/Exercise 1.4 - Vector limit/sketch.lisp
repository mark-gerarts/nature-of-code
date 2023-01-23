(defpackage :nature-of-code.vectors.exercise-4
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize))
(in-package :nature-of-code.vectors.exercise-4)

;; Since we don't use the PVector class, we'll write a function instead
(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))
