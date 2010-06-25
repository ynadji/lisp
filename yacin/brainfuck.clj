; Brainfuck interpreter written in Clojure. I was bored
; and thought it'd be fun ti write this. Soaked up some
; time and I had some fun, hooray!
;
; Yacin Nadji - yacin@gatech.edu
;
; Import, run (repl) to be dropped into a bf repl. I'll
; probably add a (compile str) function that returns
; IA 32 asm representation. That should be fun/easy.
(clojure.core/ns yacin.brainfuck)

(defn- get-loop-in-and-out
  "Gets indices for loop ins and loop outs of brainfuck code in str"
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

(defn bf-eval
  "Evaluates str, a brainfuck program."
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
	      (= (aget code i) \,) (reset! mfun #(assoc (dissoc % dp) dp (int
									  (let [in (read-line)]
									    (if (= "\\0" in) 0 (.charAt in 0))))))
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

(defn clj-to-bf
  "Compiles clojure code - code - to brainfuck."
  [code]
  (let [sb (new StringBuffer)]
    (let [fnum (nth code 1)
	  snum (nth code 2)]
      (.append sb (apply str (repeat fnum "+")))
      (.append sb ">")
      (.append sb (apply str (repeat snum "+")))
      (.append sb ">")
      (.append sb "<<")
      (.append sb "[->+<]>")
      (.append sb (apply str (repeat 48 "+")))
      (.append sb "."))
    (.toString sb)))
