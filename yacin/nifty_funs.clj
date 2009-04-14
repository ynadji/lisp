(clojure.core/ns yacin.nifty-funs
		 (:use clojure.contrib.math))

(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (round (ceil (sqrt n))))]
      (loop [i (int 3)
	     a (int-array n)
	     result (list 2)]
	(if (>= i n)
	  (reverse result)
	  (recur (+ i (int 2))
		 (if (< i root)
		   (loop [arr a
			  inc (+ i i)
			  j (* i i)]
		     (if (>= j n)
		       arr
		       (recur (do (aset arr j (int 1)) arr)
			      inc
			      (+ j inc))))
		   a)
		 (if (zero? (aget a i))
		   (conj result i)
		   result)))))))

(defn digits [n]
  "Returns a list of the digits of a number"
  (map (fn [x] (. Integer (parseInt x)))
       (rest (. (str n) (split "")))))

(defn stigid [n]
  "The opposite of digits :)"
  (. Integer (parseInt (reduce str "" n))))

(defn- factorize-out
  "Factorizes out all x factors from n.
Examples:
  (factorize-out 10 2) ==> 5, because 2^1 * 5 = 10
  (factorize-out 90 3) ==> 10, because 3^2 * 10 = 90"
  [n x]
  (loop [acc n exp 0]
    (if (= 0 (mod acc x))
      (recur (/ acc x) (inc exp))
      (hash-map :exponent exp :rest acc))))

(defn- expt-mod
  "Equivalent to (mod (expt n e) m), but faster.
http://en.wikipedia.org/wiki/Modular_exponentiation#An_efficient_method:_the_right-to-left_binary_algorithm"
  [n e m]
  (loop [r 1, b n, e e]
    (if (= e 0)
      r
      (recur (if (odd? e)
	       (mod (* r b) m)
	       r)
	     (mod (expt b 2) m)
	     (bit-shift-right e 1)))))

(defn prime?
  "Checks if n is a prime using the Miller-Rabin pseudo-primality test.  Also
   see *pseudo* and *pseudo-accuracy*."
  [n k-in]
  (cond
    (< n 2)   false
    (= n 2)   true
    (even? n) false
    :else (let [m (factorize-out (dec n) 2)
		d (:rest m)
		s (:exponent m)]
	    (loop [k 1]
	      (if (> k k-in)
		true
		(let [a (+ 2 (rand-int (- n 4)))
		      x (expt-mod a d n)]
		  (if (or (= x 1) (= x (dec n)))
		    (recur (inc k))
		    (if (loop [r 1
			       x (expt-mod x 2 n)]
			  (cond
			    (or (= x 1) (>  r (dec s)))  false
			    (= x (dec n))                true
			    :else (recur (inc r) (mod (* x x) n))))
		      (recur (inc k))
		      false))))))))