(defpackage :project-euler
  (:use :cl :nifty-funs :onlisp))
;(load "/Users/ynadji/Code/Lisp/nifty_funs.lisp")

(in-package :project-euler)
;;; Problem 1: Find the sum of all the multiples of 3 or 5
;;; below 1000

;; Brute Force approach
(defun problem-1 (n)
  (loop for x from 1 to (- n 1)
	when (or (= (mod x 3) 0) (= (mod x 5) 0))
	sum x))

;;; Problem 2: Find the num of all the even-valued terms in
;;; the Fibonacci sequence which do not exceed one million

;; Bruce Force approach
;; Note: (fib-tr 30) is the last before a million
(defun problem-2 (n)
  (loop for x from 2 to n
	for fib = (fib-tr x)
	when (evenp fib)
	sum fib))

;; Still brute force, but more lispy
(defun p2 (n)
  (reduce #'+ (remove-if-not #'evenp (mapcar #'fib-tr (list-gen 2 n)))))


;;; Problem 3: What is the largest prime factor of the number 317584931803?
;; use eratosthenes-sieve up to 3919
(defun problem-3 (n)
  (loop for x in (eratosthenes-sieve (sqrt n))
	do (if (= 0 (mod 317584931803 x))
	       (print x)
	       nil)))

;;; Smarter and more efficient way :P
(defun p-3 (n)
  (last (prime-factors n)))

;;; Problem 4: Find the largest palindrome made from the product of two 3-digit numbers.
;;; Brute-force method


;;; Problem 5: What is the smallest number that is evenly divisible by all of the numbers
;;;            from 1 to 20?
;; Solved by hand, product of prime factors for the number, numbers MUST be able to generate
;; ALL numbers from 1-20 number = (* (expt 2 4) (expt 3 2) 5 7 11 13 17 19) or be lazy and do:
;; (apply #'lcm (list-gen 1 20))

;;; Problem 6: What is the difference between the sum of the squares and
;;;            the square of the sums of the first 100 natural numbers?
(defun problem-6 (n m)
  (let ((list (list-gen n m)))
    (- (expt (apply #'+ list) 2) (apply #'+ (mapcar #'(lambda (x) (* x x)) list)))))

;;; Problem 7: Guessed and check with eratosthenes-sieve because I'm lame
;;; Problem 8: Looked for all the nines (totally lame one)

;;; Problem 9: For a<b<c and a + b + c = 1000 for the pythagorean triplet a^2 + b^2 = c^2
;;;            determine the product of a*b*c

;;; Smart way: a = m^2 - n^2; b = 2mn; c = m^2 + n^2
;;;            a + b + c = 1000 => m(m + n) = 500

;;; Brute for approach
(defun problem-9 ()
  (loop for a from 1 to 1000
	do (loop for b from 1 to 1000
		 do (loop for c from 1 to 1000
			  do (if (and (= 1000 (+ a b c)) (= (* c c) (+ (* a a) (* b b))) (< a b c))
				 (print (list a b c))
				 nil)))))

(defun problem-12 (num-div)
  (let ((hash (make-hash-table)))
    (setf (gethash 1 hash) 1)
    (labels ((tri-num (n)
	       (setf (gethash n hash) (+ n (gethash (- n 1) hash)))))
      (loop for x from 1
	    for tri = (tri-num x)
	;	  do (print tri)
	;	  do (print (divisors tri))
	    if (> (length (divisors tri)) num-div) return tri))))

(defun p-12 (num-div)
  (loop for x from 1 sum x into y
	if (> (length (divisors y)) num-div) return y))

;;; Problem 15: Find the number of paths from upper-left -> bottom-right of a 20x20 grid
;;; soln: 40 choose 20, after 20 "directions" are chosen, the remaining are placed. 40 possible routes, only need 20 to know the exact number of Lefts/Rights to get to the end.
(defun problem-15 ()
  (/ (fact-tr 40) (* (fact-tr 20) (fact-tr 20))))

;;; Problem 17: How many letters to write the words in 1 to 1000?
;;; as in => 1 = one and 134 = onehundredandthirtyfour
;;; NOTE: SBCL doesn't include and's, so those lengths are added with (* 3 891) (891 'and's total)
(defun problem-17 ()
  (+ (loop for x from 1 to 1000
	   sum (length (remove #\- (remove #\Space (format nil "~r" x))))) (* 3 891)))

;;; Helper utility for problem-18/67, stored as project-euler/tri-1.txt and tri-2.txt, respectively
;;; Parses the triangle like ((1) (2 3) (4 5 6) (7 8 9 10) ... )
(defun parse-triangle (filepath)
  (let ((triangle nil))
    (with-open-file (stream filepath)
	  (do ((line (read-line stream nil)
		     (read-line stream nil)))
	      ((null line))
	    (push (split line #\Space #'parse-integer) triangle)))
    triangle))

;;; fun problem, works from the bottom of the triangle up
;;; takes last list, finds the max values doing pair-wise comparisons
;;; i.e. (9 2 3 6) becomes (9 3 6), then sums it with the next list
;;; rinse-and-repeat
(defun problem-18/67 (filepath)
  (reduce #'(lambda (x y)
	      (mapcar #'+ (mapcar #'(lambda (a b) (max a b)) (rdc x) (cdr x)) y))
	  (parse-triangle filepath)))

;;; Problem 19: How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
;;;
;;; for i in `seq 1 12` ; do for j in `seq 1901 2000`; do cal $i $j | grep " 1  2  3  4  5  6  7"; done; done | wc -l
	 
;;; Problem 20: Find the sum of the digits in the number 100!
(defun problem-20 (n)
  (loop for x across (write-to-string (fact-tr n))
	sum (parse-integer (string x))))

;;; Problem 21: Find the sum of all amicable pairs under 10000
(defun problem-21 (n)
  (labels ((sum-div (z)
	     (reduce #'+ (remove z (divisors z)))))
    (loop for x from 1 to n
	  for a = (sum-div x)
	  for b = (sum-div a)
	  if (and (= x b) (not (= a x))) sum x)))

(defun problem-22 ()
  (with-open-file (in "/Users/ynadji/Code/Lisp/project-euler/names.txt")
    (let* ((pos 1)
	   (str (read-line in))
	   (names (split str #\, #'read-from-string))
	   (snames (sort names #'string-lessp)))
      (loop for x in snames
	   for tot = (loop for y across x sum (- (char-int y) 64))
	   sum (* pos tot)
	   do (incf pos)))))

;;; Problem 23: Find the sum of all the positive integers which
;;; cannot be written as the sum of two abundant numbers. (smaller than 28123)
;; probably works, but is ridiculously slow. figure something out that's faster
(defun problem-23 (n)
  (let ((ab-num (loop for x from 1 to n
		   when (> (reduce #'+ (remove x (divisors x))) x) collect x)))
    (loop for x from 1 to (- n 1)
       for ab-nums = (remove-if #'(lambda (y) (< x y)) ab-num)
       when (not (twosump x ab-nums)) sum x)))
;	 (ab-num-sum (loop for x in ab-num nconc (mapcar #'(lambda (y) (+ x y)) ab-num))))
;    (remove-if #'(lambda (v) (member v ab-num-sum)) ret)))

(defun twosump (val list)
  (loop for x in list do
       (loop for y in list
	  do (when (= val (+ x y)) (return-from twosump val)))))
	 
;;; Problem 25: Find the first fibonacci number 1000 digits in length
(defun problem-25 (n)
  (loop for x from 1
	do (if (eql (length (write-to-string (fib-tr x))) n)
	       (return x))))

;;; Problem 26: Find d < 1000 where 1/d has longest recurring cycle
;;; in decimal expansion (1/7 => 0.(142857))
(defun problem-26 (d)
  (let ((ret (cons 0 0))
	(tmp (cons 0 0)))
    (loop for x in (sieve5 d)
       when (>= (length (write-to-string (float (/ 1 x)))) 10)
       do (setq tmp (loop for k from 1
		       when (zerop (mod (- (expt 10 k) 1) x)) do (return (cons k x))))
       and
       do (if (> (car tmp) (car ret))
	      (setq ret tmp)))
    ret))

;;; Problem 27: Find a and b, such that n^2 + an + b creates the longest
;;; prime generation function for |a,b| < 1000
(defun problem-27 (m)
  (let* ((tmp (sieve5 m))
	 (poss-vals (append tmp (mapcar #'(lambda (x) (* -1 x)) tmp)))
	 (primes (sieve5 100000))
	 (max (cons 0 0)))
    (loop for a in poss-vals do
	 (loop for b in poss-vals
	    for v = (prime-chain-check primes a b)
	    when (> v (car max))
	    do (setq max (cons v (* a b)))))
    max))
	 
(defun prime-chain-check (prime-list a b)
  (let ((length 0))
    (loop for n from 0
       if (member (+ (expt n 2) (* a n) b) prime-list)
       do (incf length)
       else
       do (return length))))

;;; Problem 28: What is the sum of both diagonals in a 1001x1001 spiral?
(defun problem-28 (n)
  (let ((ltr-step 4)
	(curr-ot 3))
    (+ 1 (loop for x from 3 to n
	    when (oddp x) sum (- (* x x 2) ltr-step)
	    and sum (+ curr-ot curr-ot ltr-step)
	    and do (setq curr-ot (+ curr-ot ltr-step (+ ltr-step 2)))
	    and do (setq ltr-step (+ 4 ltr-step))))))

;;; Problem 29: Find all permutations of a^b for 2 <= a,b <= 100
(defun problem-29 (l u)
  (length (remove-duplicates
	   (loop for x from l to u
		append (loop for y from l to u collect (expt x y))))))

;;; Problem 30: Find the sum of all numbers that can be written as the sum of fifth powers
;;;             of their digits
(defun problem-30 ()
  (loop for x from 2 to 200000
	when (= x (reduce #'+ (mapcar #'(lambda (x) (expt x 5)) (digits x)))) sum x))

;;; Problem 31: Find the different ways to make 2 pounds from change
;;; Mathematic code
;;; SeriesCoefficient[Series[
;	(1/(1 - x)*1/(1 - x^2)*1/(1 - x^5)*1/(1 - x^10)*1/(1 - 
;        x^20)*1/(1 - x^50)*1/(1 - x^100)*1/(1 - x^200)), {x, 0, 
;	   200}], 200]
; generating function for [x^200] 1/(1-x)(1-x^2)...(1-x^200)

;;; Problem 32: The product 7254 is unusual, as the identity, 39  186 = 7254,
;;; containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;;; Find the sum of all products whose multiplicand/multiplier/product identity
;;; can be written as a 1 through 9 pandigital.

;;; NOTE: We can determine that a * b = c; a,b = 1,4 OR a,b = 2,3 digits
(defun problem-32 ()
  (let ((identities nil))
    (loop for a from 1 to 9
       do (loop for b from 1000 to 9999
	     for c = (* a b)
	     when (pandigital (stigid (list a b c)) (list-gen 1 9))
	     do (push c identities)))
    (loop for a from 10 to 99
       do (loop for b from 100 to 999
	     for c = (* a b)
	     when (and (= a 39) (= b 186))
	     when (pandigital (stigid (list a b c)) (list-gen 1 9))
	     do (push c identities)))
    (reduce #'+ (remove-duplicates identities))))

;;; Problem 34: Find the sum of the numbers that their digits' factorials, sum to the
;;;             original number
(defun problem-34 ()
  (loop for x from 1 to 999999
	when (= x (reduce #'+ (mapcar #'fact-tr (digits x)))) sum x))

;;; Problem 35: How many circular primes are there below 1,000,000?
;;; only consider numbers made up of (1 3 5 7)
(defun problem-35 (n)
  (labels ((aux (primes cnt)
	     (if (null primes)
		 cnt
		 (multiple-value-bind (np inc)
		     (circularp (car primes) primes)
		   (if (zerop inc)
		       (aux (cdr np) cnt)
			 (aux np (+ inc cnt)))))))
    ; starts at two to compensate for
    ; 2 and 5
    (aux (remove-if-not
	  #'(lambda (x) (every #'(lambda (y) (member y '(1 3 7 9))) (digits x))) (sieve5 n))
	 2)))

(defun circularp (n primes)
  (if (= 1 (length (remove-duplicates (digits n))))
      (values (cdr primes) 1)
      (let ((len (length (write-to-string n)))
	    (circulars (list n)))
	(dotimes (i (- len 1))
	  (let* ((dig (digits n))
		 (hd (pop dig)))
	    (setq n (parse-integer (format nil "~{~A~}" (append dig (list hd)))))
	    (setq circulars (cons n circulars))))
	(if (every #'(lambda (x) (member x primes)) circulars)
	    (values (remove-if #'(lambda (x) (member x circulars)) primes) (length circulars))
	    (values primes 0)))))

;;; Problem 36: Find the sum of numbers under one million that are palindromic in
;;;             both base-10 and base-2
(defun problem-36 ()
  (loop for x from 1 to 999999
	when (and (palindromep (write-to-string x)) (palindromep (format nil "~b" x))) sum x))

(defun triangularp (num)
  (let ((v (/ (- (sqrt (+ (* 8 num) 1)) 1) 2)))
    (equalp v (floor v))))

;;; Problem 39: If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
;;; there are exactly three solutions for p = 120.
;;; {20,48,52}, {24,45,51}, {30,40,50}
;;; For which value of p < 1000, is the number of solutions maximised?
(defun problem-39 (perimeter)
  (declare (fixnum perimeter) (optimize (speed 3) (safety 0)))
  (loop for a from 1 to perimeter
     if (< (+ a a (sqrt (* 2 (expt a 2)))) perimeter)
     nconc (loop for b from a to perimeter
	      for c = (multiple-value-list (floor (sqrt (+ (expt a 2) (expt b 2)))))
	      if (< (+ a b (car c)) perimeter)
	        if (zerop (cadr c))
		  collect (+ a b (car c))
		end
	      else
		do (loop-finish))
     else
     do (loop-finish)))
;	      if (and (zerop (cadr c)) (< (+ a b (car c)) perimeter))
;		collect (+ a b (car c));)))
;	      else
;	        do (loop-finish))))

;;; Problem 40: If dn represents the nth digit of the fractional part, find the value of the following expression.
;;; d1 X d10 X d100 X d1000 X d10000 X d100000 X d1000000
;;; using FORMAT is too slow, going to have to mod numbers
;;; or use DIGITS and keep track of previous digit count, and current
;;; digit count. laaaaaaaaaame

;;; Problem 42: How many words in words.new are triangular words?
(defun problem-42 ()
  (let ((words (split (car (readlines "./words.new")) #\, #'identity))
	(count 0))
    (loop for word in words
       when (triangularp (loop for y across word sum (- (char-code y) 64))) do (incf count))
    count))

;;; Problem 49: The arithmetic sequence, 1487, 4817, 8147, in which each
;;; of the terms increases by 3330, is unusual in two ways:
;;; (i) each of the three terms are prime, and,
;;; (ii) each of the 4-digit numbers are permutations of one another.
;;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
;;; exhibiting this property, but there is one other 4-digit increasing sequence.
;;; What 12-digit number do you form by concatenating the three terms in this sequence?
(defun problem-49 ()
  (labels ((member3 (perms list)
	     (let ((count 0))
	       (loop for perm in perms
		    when (member perm list :test #'equal) do (incf count))
	       (if (> count 2)
		   count
		   nil))))
    (let ((perms nil)
	  (prime-digs (remove-if #'(lambda (x) (< (length x) 4))
	   (mapcar #'digits (sieve5 9999)))))
      (loop for number in (mapcar #'digits (sieve5 9999))
	 when (and (null (member3 (permutations number) perms))
		   (member3 (permutations number) prime-digs))
	 do (push number perms))
      (setq perms (nreverse (mapcar #'stigid perms)))
	(loop for val in perms
	     when (and (member (+ val 3330) perms)
		       (member (+ val 6660) perms))
	     collect (list val (+ val 3330) (+ val 6660))))))
	 
;;; Problem 52: Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits in some order.
(defun problem-52 (num-mult)
  (let ((mult (list-gen 1 num-mult)))
    (loop for x from 1 to 1000000
	  if (apply #'list= (mapcar #'(lambda (y) (sort (digits (* x y)) #'<)) mult)) return x)))

;;; Problem 69: Find the value of n <= 1,000,000 for which n/phi(n) is a maximum, where phi(n) === (euler-totient n)
(defun problem-69 ()
  "This problem can be approach backwards. If we find the largest
   product of primes below 1,000,000; we will have the largest
   number that is relatively prime to the most numbers less than
   it. The smalles are the list of primes up to 18."
  (reduce #'* (eratosthenes-sieve 18)))

;;; Problem 79: Read in /Users/ynadji/Code/Lisp/project-euler/keylog.txt, and determine the
;;;             users password
(defun problem-79 (filepath)
  (let ((passcode nil)
	(logins nil))
    (with-open-file (stream filepath)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push (digits (parse-integer line)) logins)))
    (setf passcode (remove-duplicates (flatten logins)))
    (helper-79 passcode logins)))

(defun helper-79 (passcode logins)
  (if (null logins)
      passcode
      (let ((v1 (first (car logins)))
	    (v2 (second (car logins)))
	    (v3 (third (car logins))))
	(cond ((after v1 v2 passcode)
	       (rotatef (nth (position v1 passcode) passcode) (nth (position v2 passcode) passcode))
	       (helper-79 passcode (cdr logins)))
	      ((after v2 v3 passcode)
	       (rotatef (nth (position v2 passcode) passcode) (nth (position v3 passcode) passcode))
	       (helper-79 passcode (cdr logins)))
	      ((after v1 v3 passcode)
	       (rotatef (nth (position v1 passcode) passcode) (nth (position v3 passcode) passcode))
	       (helper-79 passcode (cdr logins)))
	      (t (helper-79 passcode (cdr logins)))))))
