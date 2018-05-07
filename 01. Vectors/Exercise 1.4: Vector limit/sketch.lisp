(defpackage :sketch (:use :cl :trivial-gamekit)
            (:import-from :cl-bodge :vector-length :normalize))
(in-package :sketch)

;; Since we don't use the PVector class, we'll write a function instead
(defun limit-vec (vec max)
  (if (> (vector-length vec) max)
      (mult (normalize vec) max)
      vec))
