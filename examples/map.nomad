#!/usr/bin/env nomad
(let my_numbers (quote (1 2 3 4 5 6 7 8 9 10)))
(let my_squared_numbers (map (lambda (x) (* x x)) my_numbers))
(let list_length (len my_squared_numbers))

(println my_squared_numbers)
(println "The list is: " list_length " elements long!")
