(clojure.core/ns yacin.euler.project-euler
		 (:use yacin.nifty-funs)
		 (:use clojure.contrib.combinatorics))

(defn problem-33 []
  "The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting
   to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
   We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
   There are exactly four non-trivial examples of this type of fraction, less than one in value,
   and containing two digits in the numerator and denominator.
   If the product of these four fractions is given in its lowest common terms, find the value of the denominator."
  (apply *
	 (loop [numer 10
		denom (inc numer)
		lst nil]
	   
	   (cond (curious-fraction? numer denom)
		 (recur numer (inc denom) (conj lst (/ numer denom))) 
		 (>= denom 99)
		 (recur (inc numer) (+ 2 numer)	lst)
		 (= 4 (count lst))
		 lst
		 true (recur numer (inc denom) lst)))))

(defn curious-reduce [nx ny]
  (try
   (let [sorted-x (sort nx)
	 sorted-y (sort ny)]
     (cond (= (first sorted-x) (first sorted-y))
	   (/ (second sorted-x) (second sorted-y))
	   (= (second sorted-x) (second sorted-y))
	   (/ (first sorted-x) (first sorted-y))))
   (catch java.lang.ArithmeticException e -1)))

(defn curious-fraction? [x y]
  (let [nx (digits x)
	ny (digits y)]
    (if (or (= x y)
	    (and (zero? (second nx))
		 (zero? (second ny))))
      false
      (= (/ x y)
	 (curious-reduce nx ny)))))

(defn problem-37 [n]
"The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove
digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work
from right to left: 3797, 379, 37, and 3. Find the sum of the only eleven primes that are both truncatable
from left to right and right to left."
  (let [primes (sieve n)]
    (loop [poss-trunc-primes primes
	   sum -17]
      (cond (empty? poss-trunc-primes)
	    sum
	    (truncatable-prime? (first poss-trunc-primes) primes)
	    (recur (rest poss-trunc-primes) (+ sum (first poss-trunc-primes)))
	    :else
	    (recur (rest poss-trunc-primes) sum)))))

(defn truncatable-prime? [x primes]
  (loop [xlist (digits x)
	 ylist (digits x)]
    (if (empty? xlist)
      true
      (if (and (some #(= (stigid xlist) %) primes)
	       (some #(= (stigid ylist) %) primes))
	(recur (reverse (rest (reverse xlist)))
	       (rest ylist))
	false))))

(defn problem-40 []
  "Finding the nth digit of the fractional part of the irrational number."
  (let [dstr (apply str (range 190000))
	pos (map #(int (Math/pow 10 %)) (range 7))]
    (reduce * (map #(Integer/parseInt (str (.charAt dstr %))) pos))))

(defn problem-41 [n]
  "What is the largest n-digit pandigital prime that exists? n = 7"
  (let [perms (permutations (reverse (range 1 (inc n))))]
    (take 10 (for [potential-prime
		  (map #(BigInteger. (apply str %)) perms)
		  :when (prime? potential-prime 10)]
	      potential-prime))))

(defn- pentagonal-number [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn- hexagonal-number [n]
  (* n (- (* 2 n) 1)))

(defn problem-45 []
  "40755 is a triangular, pentagonal and hexagonal number. When is the next one?
   All hex numbers are triangular, so check for the first hex/pent match"
  (loop [np 165, nh 144
	 p (pentagonal-number np)
	 h (hexagonal-number nh)]
    (if (= p h)
      p
      (if (< p h)
	(recur (inc np) nh
	       (pentagonal-number (inc np))
	       h)
	(recur np (inc nh)
	       p
	       (hexagonal-number (inc nh)))))))

