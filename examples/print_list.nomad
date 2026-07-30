#!/usr/bin/env nomad
(letfun print_list (l) 
  (do 
		((letfun aux (h t i) 
	  	((if (isunit h)
				(()) # unit, we are done
				(do 
				((print i)
					(print ": ")
					(println h)
					(aux ((head t) (tail t) (+ i 1))))))))

		(aux ((head l) (tail l) 0)))))

(let my_list (1 2 3 4 5 6 7 8 9 10))
(print_list (my_list))
