#!/usr/bin/env nomad
(let add 
  (lambda (x)
    (lambda (y) 
      (+ x y))))

(let add10 (add (10)))
(println (add10 (20)))
