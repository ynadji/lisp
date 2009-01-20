(use 'yacin.nifty-funs)

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
   (cond (= (first nx) (first ny))
	 (/ (second nx) (second ny))
	 (= (second nx) (second ny))
	 (/ (first nx) (first ny))
	 (= (first nx) (second ny))
	 (/ (second nx) (first ny))
	 (= (second nx) (first ny))
	 (/ (first nx) (second ny)))
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