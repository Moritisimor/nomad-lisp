#!/usr/bin/env nomad
(letfun input_loop ()
  (do
    (try
      (let x (string_to_num (readln "Enter x: ")))
    (do
      (println "Could not parse x!")
      (input_loop)))

    (try
      (let y (string_to_num (readln "Enter y: ")))
    (do
      (println "Could not parse y!")
      (input_loop)))

    (let op (readln "Enter operator: "))
    (if (= op "+")
      (println (+ x y))
    (if (= op "-")
      (println (- x y))
    (if (= op "*")
      (println (* x y))
    (if (= op "/")
      (do
        (if (= 0 y)
          (println "Cannot divide by 0!")
          (println (/ x y))))
          
      (println "Unknown operator!")))))
          
    (input_loop)))

(println "Ctrl + C to exit!")
(input_loop)
