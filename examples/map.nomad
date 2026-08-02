#!/usr/bin/env nomad
(letfun do_times (f i) 
  (do 
    (letfun aux (j)
      (if (>= j i)
        unit
        (do
          (f j)
          (aux (+ j 1)))))
          
    (aux 0)))

(letfun len (l)
  (do 
    (letfun aux (acc h t)
      (if (isunit t)
        acc
        (aux (+ acc 1) (car t) (cdr t))))

    (aux 0 (car l) (cdr l))))

(letfun rev (l)
  (do
    (letfun aux (acc h t)
      (if (isunit t)
        acc
        (aux (cons h acc) (car t) (cdr t))))
        
    (aux () (car l) (cdr l))))

(letfun map (f l)
  (do
    (letfun aux (acc h t)
      (if (isunit t)
        (rev acc)
        (aux (cons (f h) acc) (car t) (cdr t))))
        
    (aux () (car l) (cdr l))))

(letfun square (x) (* x x))
(let my_numbers (quote (1 2 3 4 5 6 7 8 9 10)))
(let my_squared_numbers (map (lambda (x) (* x x)) my_numbers))
(let list_length (len my_squared_numbers))

(println my_squared_numbers)
(println "The list is: " list_length " elements long!")

(do_times (lambda (x) (println "Hello Nr. " x)) 10)
