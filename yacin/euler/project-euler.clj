(clojure.core/ns yacin.euler.project-euler
		 (:use yacin.nifty-funs)
		 (:use clojure.contrib.math)
		 (:use clojure.contrib.combinatorics)
		 (:use clojure.contrib.str-utils)
		 (:use clojure.contrib.pprint))

;;;; constants
(def input-file-59 "/Users/ynadji/Code/Lisp/yacin/euler/cipher1.txt")

(defn- curious-reduce
  "Helper for problem-33"
  [nx ny]
  (try
   (let [sorted-x (sort nx)
	 sorted-y (sort ny)]
     (cond (= (first sorted-x) (first sorted-y))
	   (/ (second sorted-x) (second sorted-y))
	   (= (second sorted-x) (second sorted-y))
	   (/ (first sorted-x) (first sorted-y))))
   (catch java.lang.ArithmeticException e -1)))

(defn- curious-fraction?
  "Helper for problem-33"
  [x y]
  (let [nx (digits x)
	ny (digits y)]
    (if (or (= x y)
	    (and (zero? (second nx))
		 (zero? (second ny))))
      false
      (= (/ x y)
	 (curious-reduce nx ny)))))

(defn problem-33
  "The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting
   to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
   We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
   There are exactly four non-trivial examples of this type of fraction, less than one in value,
   and containing two digits in the numerator and denominator.
   If the product of these four fractions is given in its lowest common terms, find the value of the denominator."
  []
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

(defn- truncatable-prime?
  "Helper for problem-37"
  [x primes]
  (loop [xlist (digits x)
	 ylist (digits x)]
    (if (empty? xlist)
      true
      (if (and (primes (stigid xlist))
	       (primes (stigid ylist)))
	(recur (reverse (rest (reverse xlist)))
	       (rest ylist))
	false))))

(defn problem-37
  "The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove
digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work
from right to left: 3797, 379, 37, and 3. Find the sum of the only eleven primes that are both truncatable
from left to right and right to left."
  [n]
  (let [primes (apply hash-map (interleave (sieve n) (repeat true)))]
    (loop [poss-trunc-primes (keys primes)
	   sum -17]
      (cond (empty? poss-trunc-primes)
	    sum
	    (truncatable-prime? (first poss-trunc-primes) primes)
	    (recur (rest poss-trunc-primes) (+ sum (first poss-trunc-primes)))
	    :else
	    (recur (rest poss-trunc-primes) sum)))))

(defn- pandigital?
  "Helper for problem-38"
  [n]
  (= (range 1 10) (sort (digits n))))

(defn- get-pandigital
  "Helper for problem-38"
  [n]
  (loop [end 2]
    (let [num (read-string (apply str (map #(* % n) (range 1 end))))
	  len (count (digits num))]
      (cond (> len 9)
	    false
	    (and (= len 9)
		 (pandigital? num))
	    num
	    :else
	    (recur (inc end))))))

(defn problem-38
  "What is the largest 1 to 9 pandigital 9-digit number that can be
   formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?"
  [start end]
  (loop [n start, max 0, maxn 0]
    (if (= n end)
      [max maxn]
      (let [next-val (get-pandigital n)]
	(if (and next-val
		 (> next-val max))
	  (recur (inc n) next-val n)
	  (recur (inc n) max maxn))))))

(defn problem-40
  "Finding the nth digit of the fractional part of the irrational number."
  []
  (let [dstr (apply str (range 190000))
	pos (map #(expt 10 %) (range 7))]
    (reduce * (map #(Integer. (str (.charAt dstr %))) pos))))

(defn problem-41
  "What is the largest n-digit pandigital prime that exists? n = 7"
  [n]
  (let [perms (permutations (reverse (range 1 (inc n))))]
    (take 10 (for [potential-prime
		  (map #(BigInteger. (apply str %)) perms)
		  :when (prime? potential-prime 10)]
	      potential-prime))))

(defn- pentagonal-number
  "Helper for problem-45"
  [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn- hexagonal-number
  "Helper for problem-45"
  [n]
  (* n (- (* 2 n) 1)))

(defn problem-45
  "40755 is a triangular, pentagonal and hexagonal number. When is the next one?
   All hex numbers are triangular, so check for the first hex/pent match"
  []
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

(defn problem-53
  "How many, not necessarily distinct, values of C(n,r), for 1 <= n <= 100, are greater than one-million?
   Solution: (problem-53 23). The problem states C(23,10) is the first val >= 1 * 10^6."
  [n]
  (loop [n n, r 1, cnt 0]
    (cond (> n 100)
	  cnt
	  (> (choose n r) 1000000)
	  (let [mid (/ n 2)
		diff (+ 1 mid (- mid r))
		incz (- diff r)]
	    (recur (inc n) 1 (+ cnt incz)))
	  (= n r)
	  (recur (inc n) 1 cnt)
	  :else
	  (recur n (inc r) cnt))))

;;;; doesn't work
(defn problem-59
  []
  (let [input (slurp input-file-59)
	ascii-chars (map #(Integer. (chomp %)) (re-split #"," input))]
    (loop [xor-value 0]
      (let [new-ascii (map #(bit-xor xor-value %) ascii-chars)
	    string (apply str (map char new-ascii))]
	(cl-format true "~a~%" string)
	(if (or (> xor-value 255)
		(or (re-find #" the " string)
		    (re-find #" THE " string)))
	  string
	  (recur (inc xor-value)))))))