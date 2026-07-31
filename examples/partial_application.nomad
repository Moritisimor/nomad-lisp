#!/usr/bin/env nomad
(let add 
  (λ (x) # λ is optional, simply typing 'lambda' is just as valid.
    (λ (y)
      (+ x y))))

(let add10 (add 10))
(let x (add10 20))
(println (+ "x = " (to_string x)))
(if (= x 30)
  (println "All good! Closures work as expected.")
  (println "Uh oh, Closures do not work as expected!"))
