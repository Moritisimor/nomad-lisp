# nomad-lisp
![Nomad Lisp Logo](docs/NomadLogo.svg)

Nomad is a small, readable, dynamically typed Lisp written in OCaml. It comes with a REPL, a practical standard library, macros, closures, persistent lists, mutable records, file and OS helpers, and a straightforward API for embedding the interpreter.

## What is this project about?
This is my own Lisp dialect: compact enough to understand, but complete enough to write useful scripts. The OCaml implementation is the original Nomad, and its behavior is kept in sync with the Go and Rust ports.

Evaluation is stack-safe for tail calls and core forms such as `if`, `do`, `switch`, `scoped`, and `try`. Long-running recursive programs do not have to trade the simple Lisp style for an imperative loop.

You can find markdown files that serve as documentation [here](https://github.com/Moritisimor/nomad-lisp/tree/main/docs).

The portable language and implementation requirements are defined by [The Nomad Lisp Language Standard](reference/standard.md). The original [informal reference](reference/reference.md) is kept as design background.

You can also find some example programs [here](https://github.com/Moritisimor/nomad-lisp/tree/main/examples).

## Other implementations
Check out the other implementations!

The nomad implementation in go, designed for embedding within go applications:

[gomad](https://github.com/Moritisimor/gomad)

The nomad implementation in rust by [RobertFlexx](https://github.com/RobertFlexx):

[romad](https://github.com/RobertFlexx/romad)

The nomad implementation in gleam for the BEAM by [RobertFlexx](https://github.com/RobertFlexx):

[bomad](https://github.com/RobertFlexx/bomad)

If you want a port of nomad to the full DotNet 10.0 ecosystem, check out **this**.

[DotMad](https://github.com/RobertFlexx/DotMad)

## Cloning and building
### Prerequisites
You will need OCaml 5.4.1 or newer, opam, dune, and git.

### Script
```bash
git clone https://github.com/Moritisimor/nomad-lisp
cd nomad-lisp
opam install . --deps-only
eval $(opam env)
make
cp _build/default/bin/main.exe nomad
```

You can now enter the REPL like this:
```bash
./nomad
```

Run the conformance and tail-call tests with `make test`. Use `make clean` to remove dune's build output.

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
```lisp
(bye)
```

### Examples
#### Fibonacci
```lisp
(letfun fib (n)
  (switch n
    (0 0)
    (1 1)
    (_ (+ (fib (dec n)) (fib (- n 2))))))

(let x 20)
(println (fib x))
```

#### Factorial
```lisp
(letfun fact (n)
  (switch n
    (0 1)
    (_ (* n (fact (dec n))))))

(let x 10)
(println (fact x))
```
#### Macros
```lisp
(letmac when (cond body) # This macro is already in the standard library
  if cond body unit)

(when true (println "I will always be printed!"))
```

#### A recursive counter function
```lisp
(letfun count (start end)
  (if (< end start)
    (println "done!")
    (do 
      (println start) 
      (count (+ start 1) end))))

(let start 0)
(let end 10)
(count start end)
```

#### Currying and Function Composition in action
```lisp
(let add 
  (lambda (x)
    (lambda (y)
      (+ x y))))

(let add10 (add 10))
(let x (add10 20))
(println "x = " x)
(if (= x 30)
  (println "All good! Closures work as expected.")
  (println "Uh oh, Closures do not work as expected!"))
```

#### A Quiz Application
```lisp
(letfun input_loop (correct_answer prompt)
  (if (= correct_answer (readln prompt))
    (println "Correct!")
    (do 
      (println "False!\nTry again!")
      ((input_loop correct_answer prompt)))))

(input_loop "berlin" "What is the capital of Germany? ")
(input_loop "21" "What's 9 + 10? ")
(input_loop "ocaml" "What language is Nomad-LISP written in? ")
(println "All questions answered correctly!")
```

#### Recursive helper functions
```lisp
(letfun print_list (l)
  (do
    (letfun aux (h t i)
      (if (isunit t)
        unit
        (do
          (println i ": " h)
          (aux (car t) (cdr t) (+ i 1)))))
        
    (aux (car l) (cdr l) 0)))

(let my_list (list 1 2 3 4 5 6 7 8 9 10))
(print_list my_list)
```
