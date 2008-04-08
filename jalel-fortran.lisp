;;;; apparently (isqrt n) always returns the same answer...
;;;; unfortunately, i haven't the slightest idea why
(defun fortran (n)
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((cells (make-array n :element-type 'bit :initial-element 1)))
    (loop for x from 2 to n
	 do (dotimes (pos n)
	      (if (zerop (mod (+ pos 1) x)) (setf (sbit cells pos) (logxor (sbit cells pos) 1)))))
    (loop for x across cells sum x)))
