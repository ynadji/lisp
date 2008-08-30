(defpackage :nifty-funs
  (:use :cl)
  (:export #:full-mapcar)
  (:export #:rdc)
  (:export #:fact-tr)
  (:export #:fib-tr)
  (:export #:list-gen)
  (:export #:average)
  (:export #:prime-factors)
  (:export #:eratosthenes-sieve)
  (:export #:sieve5)
  (:export #:palindromep)
  (:export #:digits)
  (:export #:stigid)
  (:export #:divisors)
  (:export #:split)
  (:export #:list=)
  (:export #:readlines)
  (:export #:euler-totient)
  (:export #:pandigital)
  (:export #:permutations)
  (:export #:choose)
  (:export #:perms))

(provide :nifty-funs)
(in-package :nifty-funs)

;;; fully recursive implementation of mapcar
(defun full-mapcar (function list)
  (cond ((null list) nil)
	((listp (car list)) (cons (full-mapcar function (car list)) (full-mapcar function (cdr list))))
	(t (cons (funcall function (car list)) (full-mapcar function (cdr list))))))

;;; opposite of cdr
(defun rdc (list)
  (remove (car (last list)) list :from-end t :count 1))

(defun fact-tr (n &optional (acc 1))
  (if (<= n 1)
      acc
      (fact-tr (- n 1) (* acc n))))

(defun fib-tr (n &optional (next 1) (current 0))
  (if (= 0 n)
      current
      (fib-tr (- n 1) (+ current next) next)))

(defun average (list)
  (/ (loop for x in list sum x) (length list)))

(defun list-gen (start end)
  (loop for x from start to end collecting x))

(defun prime-factors (n)
  (when (> n 1)
    (do ((x 2 (1+ x)))
	((zerop (mod n x))
	 (cons x (prime-factors (/ n x)))))))

(defun eratosthenes-sieve (n &optional (composites nil) (primes nil))
  (cond ((and (null composites) (null primes))
	 (eratosthenes-sieve n (loop for x from 2 to n collecting x)))
	((null composites) primes)
	(t (let ((new-prime (car composites)))
	     (eratosthenes-sieve n (remove-if #'(lambda (x) (if (= 0 (mod x new-prime)) t nil)) composites) (cons new-prime primes))))))

;; Roger Corman's Sieve function from Corman Lisp examples
(defun sieve5 (n)
  "Returns a list of all primes from 2 to n"
  (declare (fixnum n) (optimize (speed 3) (safety 0)))
  (let* ((a (make-array n :element-type 'bit :initial-element 0))
	 (result (list 2))
	 (root (isqrt n)))
    (declare (fixnum root))
    (do ((i 3 (the fixnum (+ i 2))))
	((>= i n) (nreverse result))
      (declare (fixnum i))
      (progn (when (= (sbit a i) 0)
	       (push i result)
	       (if (< i root)
		   (do* ((inc (+ i i))
			 (j (* i i) (the fixnum (+ j inc))))
			((>= j n))
		     (declare (fixnum j inc))
		     (setf (sbit a j) 1))))))))

(defun erat (n)
  (let ((primes (list-gen 2 n)))
    (loop for x in primes
       for nprime = (pop primes)
       collecting nprime
       do (setq primes (remove-if #'(lambda (y) (= (mod y nprime) 0)) primes)))))

(defun newerat (n)
  (labels ((aux (primes composites)
	     (if (null composites)
		 primes
		 (aux (cons (car composites) primes) (remove-if #'(lambda (y) (= (mod y (car composites)) 0)) composites)))))
    (aux '() (list-gen 2 n))))

(defun palindromep (string)
  (cond ((= 0 (length string)) t)
	((= 1 (length string)) t)
	((eql (elt string 0) (elt string (- (length string) 1)))
	 (if (= 2 (length string))
	     t
	     (palindromep (subseq string 1 (- (length string) 1)))))))

;;; Less efficient, simpler though
(defun rpalindromep (string)
  (string= string (reverse string)))

(defun digits (num)
  (map 'list #'(lambda (char) (read-from-string (string char))) (prin1-to-string num)))

(defun stigid (list)
  (values (parse-integer (format nil "~{~a~^~}" list))))

(defun divisors (x)
  (remove-duplicates
   (loop for y from 1 to (sqrt x)
	for z = (/ x y)
	when (integerp z) collect y and collect z)))

;;; Doesn't work perfectly yet, improperly parses strings with multiple delimiters
;;; (split "1,,,3" #\, #'parse-integer) SHOULD return (1 nil nil 3) but returns (1 3)
(defun split (line delim parse-as)
  (let ((lst nil))
    (labels ((helper (line delim parse-as lst)
	       (cond ((string= line "") lst)
		     (t
		      (let ((idx (position delim line)))
			(push (funcall parse-as (subseq line 0 idx)) lst)
			(if (null idx)
			    (nreverse lst)
			    (helper (string-trim (write-to-string delim) (subseq line idx))
				    delim parse-as lst)))))))
      (helper line delim parse-as lst))))

;(defun list= (lst &rest more-lists &key (test #'=))
(defun list= (lst &rest more-lists)
;  (print more-lists)
  (if (null (car more-lists))
      t
      (if (equal lst (car more-lists))
	  (list= lst (car (cdr more-lists)))
	  nil)))
;      (mapc #'(lambda (x &rest y)
;		(if (not (apply #'= (cons x y))) (return-from list= nil))) lst (apply #'append more-lists))))

;;;; Stuff from CS 440

(defun pset (lst)
  (if (null lst) '(())
      (let ((hd (car lst))
	    (tl (pset (cdr lst))))
	(append tl (mapcar #'(lambda (ll) (cons hd ll)) tl)))))

(defun pset-tr (lst)
  (labels ((aux (lst nlst)
	     (if (null lst)
		 nlst
		 (let ((hd (car lst))
		       (tl (cdr lst)))
		   (aux tl (append nlst (mapcar #'(lambda (ll) (cons hd ll)) nlst)))))))
  (aux lst '(()))))

(defun permutations (list)
  "Return a list of all the permutations of the input."
  (if (null list)
      '(())
      (mapcan #'(lambda (e)
		  (mapcar #'(lambda (p) (cons e p))
			  (permutations
			   (remove e list :count 1))))
	      list)))

(defun readlines (path &optional (fn #'identity))
  "Sucks up an entire file from PATH into a list of freshly-allocated
      strings, returning two values: the list of strings and the number of
      lines read."
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
       and line-count from 0
       while line
       collect (funcall fn line) into lines
       finally (return (values lines line-count)))))

(defun euler-totient (n)
  "Returns the euler-totient, and the coprimes that count the totient
   for any number n"
  (let ((coprimes (loop for x from 1 to n when (= 1 (gcd x n)) collect x)))
    (values (length coprimes) coprimes)))

(defun part-k-n (k n)
  (let ((cache (make-hash-table :test #'equal)))
    (labels ((aux (k n cache)
	       (multiple-value-bind (val win) (gethash (list k n) cache)
		 (if win
		     val
		     (setf (gethash (list k n) cache)
			   (cond ((> k n) 0)
				 ((= k n) 1)
				 (t (+ (part-k-n (+ k 1) n) (part-k-n k (- n k))))))))))
    (aux k n cache))))

(defun pandigital (n digits)
  (cond ((not (= (length digits) (length (digits n))))
	 nil)
	(t
  (loop for x in (digits n)
       do (setq digits (delete x digits)))
  (if (null digits)
      t
      nil))))

(defun choose (n r)
  (/ (fact-tr n) (* (fact-tr (- n r)) (fact-tr r))))

(defun perms (n r)
    (/ (fact-tr n) (fact-tr (- n r))))