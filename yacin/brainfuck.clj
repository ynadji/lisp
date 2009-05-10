(clojure.core/ns yacin.brainfuck)

(defn- get-loop-in-and-out
  "Gets indices for loop ins and loop outs"
  [str]
  (loop [code (into-array str)
	 tmp nil
	 i 0
	 loop-in {}
	 loop-out {}]
    (if (= i (count code))
      [loop-in loop-out code]
      (cond (= (aget code i) \[)
	    (recur code (conj tmp i) (inc i)
		   loop-in loop-out)
	    (= (aget code i) \])
	    (recur code (rest tmp) (inc i)
		   (assoc loop-in i (first tmp))
		   (assoc loop-out (first tmp) i))
	    :else
	    (recur code tmp (inc i)
		   loop-in loop-out)))))

(defn- bf-eval
  "Evaluates a line of brainfuck."
  [str]
  (let [[loop-in loop-out code] (get-loop-in-and-out str)]
    (loop [dp 0
	   m {}
	   i 0
	   out (new StringBuffer "")]
      (cond (= i (count code))
	    (print (.toString out))
	    (= (aget code i) \>)
	    (recur (inc dp) m (inc i) out)
	    (= (aget code i) \<)
	    (recur (dec dp) m (inc i) out)
	    (= (aget code i) \+)
	    (recur dp (assoc (dissoc m dp) dp (mod (inc (or (get m dp) 0)) 255))
		   (inc i) out)
	    (= (aget code i) \-)
	    (recur dp (assoc (dissoc m dp) dp (mod (dec (or (get m dp) 0)) 255))
		   (inc i) out)
	    (= (aget code i) \.)
	    (recur dp m (inc i) (. out append (char (get m dp))))
	    (= (aget code i) \[)
	    (recur dp m (inc (if (zero? (get m dp)) (get loop-out i) i)) out)
	    (= (aget code i) \])
	    (recur dp m (get loop-in i) out)
	    :else
	    (recur dp m (inc i) out)))))

(defn repl
  "Line-by-line interpreter for brainfuck."
  []
  (print "> ")
  (flush)
  (let [str (read-line)]
    (if (= str "exit")
      nil
      (do (bf-eval str)
	  (flush)
	  (recur)))))