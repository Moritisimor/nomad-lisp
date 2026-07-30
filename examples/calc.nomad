#!/usr/bin/env nomad
(letfun input_loop (_)
  (do
		((let x (string_to_num (readln "Enter x: ")))
		(let y (string_to_num (readln "Enter y: ")))
		((if (or (iserr x) (iserr y))
			(do
				((println "Couldn't parse x and/or y!")
				(input_loop (()))))
			(do
				((let op (readln "Enter operator: "))
				(if (= op "+")
					(println (+ x y))
				(if (= op "-")
					(println (- x y))
				(if (= op "*")
					(println (* x y))
				(if (= op "/")
					(println (/ x y))
					(do
						((println "Unknown operator!")
						(input_loop (())))))))))))))
						
		(input_loop (()))))

(println "Ctrl + C to exit!")
(input_loop (()))
