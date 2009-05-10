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
    (loop [dp 0, m {}, i 0, out (new StringBuffer "")]
      (let [dpfun (atom identity)
	    mfun (atom identity)
	    ifun (atom inc)
	    outfun (atom identity)]
	(if (= i (count code))
	  (print (.toString out))
	  (do
	    (cond
	      (= (aget code i) \>) (reset! dpfun inc)
	      (= (aget code i) \<) (reset! dpfun dec)
	      (= (aget code i) \+) (reset! mfun #(assoc (dissoc % dp) dp (mod (inc (or (get % dp) 0)) 255)))
	      (= (aget code i) \-) (reset! mfun #(assoc (dissoc % dp) dp (mod (dec (or (get % dp) 0)) 255)))
	      (= (aget code i) \.) (reset! outfun #(. % append (char (get m dp))))
	      (= (aget code i) \[) (reset! ifun #(inc (if (zero? (get m dp)) (get loop-out %) %)))
	      (= (aget code i) \]) (reset! ifun #(get loop-in %)))
	    (recur (@dpfun dp) (@mfun m) (@ifun i) (@outfun out))))))))

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