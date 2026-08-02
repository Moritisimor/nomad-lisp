#!/usr/bin/env nomad
(letfun input_loop ()
  (do
    (let x (string_to_num (readln "Enter x: ")))
    (let y (string_to_num (readln "Enter y: ")))
    (if (or (iserr x) (iserr y))
      (println "Couldn't parse x and/or y!")
      (do
        (let op (readln "Enter op: "))
        (if (= op "+")
          (println (+ x y))
        (if (= op "-")
          (println (- x y))
        (if (= op "*")
          (println (* x y))
        (if (= op "/")
          (if (= y 0)
            (println "Cannot divide by zero!")
            (println (/ x y)))
            
          (println "Unknown operator!")))))))
          
    (input_loop)))

(println "Ctrl + C to exit!")
(input_loop)
