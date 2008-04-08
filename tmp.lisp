(defun range (start stop)
  (if (= start stop)
      (list start)
      (cons start (range (+ start 1) stop))))

(defun filter (pred L)
  (if (null L)
      '()
      (if (funcall pred (car L))
	  (cons (car L) (filter pred (cdr L)))
	  (filter pred (cdr L)))))

(defun main (limit)
  (mapcar (lambda (x) (prin1 x) (princ " ")) (generatePrimes (range 2 limit) limit))
  '())

(defun generatePrimes (primes limit)
  (if (> (* (car primes) (car primes)) limit)
      primes
      (cons (car primes) (generatePrimes (filter (lambda (x) (not (= (mod x (car primes)) 0))) (cdr primes)) limit))))