;; (ns jfdi (:use clojure.pprint))

(defn add [x y] (+ x y))
(defn sub [x y] (- x y))
(defn mul [x y] (* x y))
(defn exec [fn x y] (fn x y))

;; testing
(exec add 4 5) ;; 9
(exec sub 4 5) ;; -1
(exec mul 4 5) ;; 20
