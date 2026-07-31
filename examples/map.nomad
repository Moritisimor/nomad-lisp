#!/usr/bin/env nomad
(letfun map (f l)
  (do
    ((letfun aux (acc h t) 
      (if (isunit t)
        acc
        (aux ((append acc ((f (h)))) (head t) (tail t)))))

    (aux (() (head l) (tail l))))))

(let my_numbers (1 2 3 4 5 6 7 8 9 10))
(println (map ((lambda (x) (* x x)) my_numbers)))
