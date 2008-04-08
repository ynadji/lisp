(defun sum-int (a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b))))
(defun sum (a b)
  (+ a b))

(defun sum-sq (a b)
  (if (> a b)
      0
      (+ (square a) (sum-sq (1+ a) b))))

(defun square (a)
  (* a a))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

(defun sq-rt (x)
  (fixed-point
   (lambda (y) (average (/ x y) y))
   1))