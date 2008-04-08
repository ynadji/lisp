(defun accumulate (combiner null-value l)
  (if (null l)
      null-value
      (funcall combiner (car l)
	       (accumulate combiner null-value (cdr l)))))

(defun square (x)
  (* x x))

(defun product-of-squares (list)
  (accumulate #'+ 0 (mapcar #'square list)))