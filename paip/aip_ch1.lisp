(defpackage :aip-ch1
  (:use :cl)
  (:export #:mappend))

(provide :aip-ch1)
(in-package :aip-ch1)

(defun power (base exp)
  (if (= exp 0)
      1
      (* base (power base (- exp 1)))))

(defun dot-product (elem1 elem2)
  (+ (* (first elem1) (first elem2)) (* (second elem1) (second elem2))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))