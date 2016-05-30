(defn factorial [n]
	(if (= n 1)
		1
		(* n (factorial (- n 1)))))

(defn sum-digits [radix n]
	(if (= n 0)
		0
		(+ (rem n radix) (sum-digits radix (quot n radix)))))
	

(println (sum-digits 10 (factorial 100)))
