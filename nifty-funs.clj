(defn sieve [#^Integer n]
  "Returns a list of all primes from 2 to n"
  (let [root (Math/round (Math/floor (Math/sqrt n)))]
    (loop [#^Integer i 3
	   a (make-array Boolean n)
	   result (list 2)]
      (if (>= i n)
	(reverse result)
	(recur (+ i 2)
	       (if (< i root)
		 (loop [arr a
			#^Integer inc (+ i i)
			#^Integer j (* i i)]
		   (if (>= j n)
		     arr
		     (recur (do (aset arr j true) arr)
			    inc
			    (+ j inc))))
		 a)
	       (if (not (aget a i))
		 (conj result i)
		 result))))))

(defn unoptimized-sieve [n]
  "Returns a list of all primes from 2 to n"
  (let [root (Math/round (Math/floor (Math/sqrt n)))]
    (loop [i 3
	   a (make-array Boolean n)
	   result (list 2)]
      (if (>= i n)
	(reverse result)
	(recur (+ i 2)
	       (if (< i root)
		 (loop [arr a
			inc (+ i i)
			j (* i i)]
		   (if (>= j n)
		     arr
		     (recur (do (aset arr j true) arr)
			    inc
			    (+ j inc))))
		 a)
	       (if (not (aget a i))
		 (conj result i)
		 result))))))