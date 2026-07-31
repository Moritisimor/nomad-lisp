#!/usr/bin/env nomad
(letfun map (f l)
  (do
    (letfun aux (acc h t)
      (if (isunit t)
        acc
        (aux (cons acc (f h)) (car t) (cdr t))))
        
    (aux () (car l) (cdr l))))

(let my_numbers (quote (1 2 3 4 5 6 7 8 9 10)))
(letfun square (x) (* x x))
(println (map square my_numbers))
