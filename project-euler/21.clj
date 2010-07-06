(defn factors [n]
	(filter #(zero? (mod n %))
		(take (- n 1) (iterate inc 1))))

(defn d [n]
	(reduce + (factors n)))

(defn amicable? [a b]
	(and
		(= a (d b))
		(= (d a) b)))

(defn list-length [a]
	(if (= (first a) nil)
		0
		(+ 1 (list-length (rest a)))))
