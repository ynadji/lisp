(defn hello []
  (printf "hello!\n"))

(defn plus
  ([] 0)
  ([x] x)
  ([x y] (+ x y))
  ([x y & rest] (plus (plus x y) (plus rest))))