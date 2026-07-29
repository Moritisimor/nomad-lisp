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
```bash
(+ 1 2)
(* 6 7)
(+ (* 10 5) (- 1000 250))
```

You can also try this tiny program that greets you!
```bash
(println (+ "Hello, " (readln "Enter your name: ")))
```

To exit:
```bash
(exit)
```
