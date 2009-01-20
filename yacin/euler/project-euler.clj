;;;; Project Euler, Clojure version!
(clojure.core/ns yacin.euler.project-euler
		 (:use yacin.nifty-funs))

(defn test [n]
  (digits n))

(defn main
  (test 473289473289))