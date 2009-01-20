(clojure.core/ns yacin.nifty-funs)

(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
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
  (map (fn [x] (. Integer (parseInt x)))
       (rest (. (str n) (split "")))))