(defun polyfun (p)
  (cond ((null p) nil)
	(t (eval (list 'lambda '(x) (cons '+ (monolist p)))))))

; returns a list of monomials
(defun monolist (ll)
  (cond ((null ll) nil)
	(t (cons (append-xs (car (makelists ll)) (length (cdr ll)))
		 (monolist (cdr ll))))))

(defun makelists (ll)
  (cond ((null ll) nil)
	      (t (cons (cons '* (list (car ll))) (makelists (cdr ll))))))

; appends n xs to the list ll
(defun append-xs (ll n)
  (cond ((not (numberp n)) ll)
	((= 0 n) ll)
	(t (append (append-xs ll (- n 1)) (list 'x)))))

(defmacro polynomial (list x)
  )

(defun poly (list x)
  (if (null list)
      0
      (+
       (* x (expt (car list) (- (length list) 1)))
       (poly (cdr list) x))))