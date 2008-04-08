;;;; Application #1: Lazy Evaluation
(defconstant unforced (gensym))
(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
	     #'(lambda ()
		 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
	  (funcall (delay-closure x))
	  (delay-forced x))
      x))

(defun lazy-cdr (list)
  (setf (cdr list) (force (cdr list))))

(defmacro lazy-cons (head tail)
  `(cons ,head (delay ,tail)))

(defun numbers (&optional (start 1) (end nil))
  (unless (and end (> start end))
    (lazy-cons start (numbers (1+ start) end))))

(defun primes ()
  (labels ((filter-multiples-of (n list)
	     (if (zerop (mod (car list) n))
		 (filter-multiples-of n (lazy-cdr list))
		 (lazy-cons (car list) (filter-multiples-of n (lazy-cdr list)))))
	   (primes-from (n)
	     (lazy-cons n (filter-multiples-of n (primes-from (1+ n))))))
    (primes-from 2)))

(defun get-primes (n)
  (labels ((aux (n primes total)
	     (if (< n 1)
		 (nreverse total)
		 (aux (- n 1) (lazy-cdr primes) (cons (car primes) total)))))
    (aux n (primes) nil)))