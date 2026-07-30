# nomad-lisp
Modern, readable, dynamically typed, interpreted LISP dialect written in OCaml.

## What is this project about?
This is my very own LISP Dialect.

The interpreter is written in OCaml.

nomad-lisp is still in its very early stages in development. More to come soon.

## Cloning and building
### Prerequisites
You will need the `dune` build system installed as well as `git`.

### Script
```bash
git clone https://github.com/Moritisimor/nomad-lisp
cd nomad-lisp
dune build
cp _build/default/bin/main.exe nomad
```

You can now enter the REPL like this:
```bash
./nomad
```

Try typing some basic arithmetics and play around!
```lisp
(+ 1 2)
(* 6 7)
(+ (* 10 5) (- 1000 250))
```

You can also try this tiny program that greets you!
```lisp
(println (+ "Hello, " (readln "Enter your name: ")))
```

To exit:
```bash
(exit)
```

### Examples
#### A recursive counter function
```lisp
(letfun count (start end)
  (if (< end start)
    (println "done!")
    (do 
      ((println start) 
      (count ((+ start 1) end))))))

(count (0 10))
```

#### Currying and Function Composition in action
```lisp
(let add 
  (lambda (x)
    (lambda (y) 
      (+ x y))))

(let add10 (add (10)))
(let z (add10 (20)))
(println (+ "z = " (to_string z)))
(if (= z 30)
  (println "All good! Closures work as expected.")
  (println "Uh oh, Closures do not work as expected!"))
```

#### A Quiz Application
```lisp
(letfun input_loop (correct_answer prompt)
  (if (= correct_answer (readln prompt))
    (println "Correct!")
    (do 
      ((println "False!\nTry again!")
      ((input_loop (correct_answer prompt)))))))

(input_loop ("berlin" "What is the capital of Germany? "))
(input_loop ("21" "What's 9 + 10? "))
(input_loop ("ocaml" "What language is Nomad-LISP written in? "))
```

#### Recursive helper functions
```lisp
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
```
