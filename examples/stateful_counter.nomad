#!/usr/bin/env nomad
(letfun make_counter ()
  (do
    (let x 0)
    (lambda ()
      (do
        (mut x (+ x 1))
        x))))

(let c (make_counter))

(println (c))
(println (c))
(println (c))
# prints 3!
