# Nomad Lisp
Nomad Lisp is a Lisp-like language, meaning it uses S-Expressions that are homoiconic (code is data).

It's not a particularly beginner-friendly language. Nomad expects you to know some programming concepts such as...
- Immutability
- Higher order functions
- Homoiconicity
- Recursion

However, Nomad isn't particularly feature-packed either and its standard library is rather small.

## General Syntax
All functions are applied in the same way. 

The syntax looks as follows:
```lisp
# This is a comment
(<function expression> <function arguments...>)
```

Constructs that would be special syntax in other languages are simply functions in Nomad Lisp.

This works because Nomad's internal embedding API for OCaml works by passing raw S-Expressions to the callbacks, not evaluated values.

As a consequence of this architecture, functions such as `+`, `-`, `*`, `/` and theoretically even those such as `if`, `let` etc. are regular functions that may be passed around. 

However, I am unsure how much sense it makes to pass around `if` as a higher order function.

## About the language
Nomad is an expression-oriented language, meaning everything returns something.

There is no `void` like in Java, instead, nomad uses the `unit` type.

Those of you that know languages such as Rust or OCaml will know what `unit` is.

`Unit` is a type like any other, except that it indicates that there is no value that makes sense.

Generally, it is used to distinguish functions that return a value from procedures that do not. However, it is also used by `car` and `cdr` to indicate that a list is empty.

## Variables
To bind a variable, you use the `let` function.

This is not to be confused with traditional Lisp's `let`. This is called `scoped` in nomad.

### Example
```lisp
(let x 10)
(let y 20)
(println (+ x y)) # 30
```

## Functions
Generally, you will want to use the `letfun` function for binding functions to variables.

### Example
```lisp
(letfun greet (name) (println "Hello, " name))
(let person "John Doe")
(greet person) # "Hello, John Doe"
```

However, it is also possible to use regular `let` and bind a lambda to it.

### Example
```lisp
(let greet (lambda (name) (println "Hello, " name)))
(let person "John Doe")
(greet person) # "Hello, John Doe"
```

## If
Nomad's expression-oriented syntax means that constructs like `if` are expressions, not just control-flow.

This means that code like this is possible:
```lisp
(println
  (if true
    "True!"
    "False!"))
```

This `if` itself is an expression that gets evaluated and its result is passed onto the `println`.

Whereas in Python you'd write it more like this:
```python
if True:
  print("True!")
else:
  print("False!")
```

## Do
Usually, constructs like `if` or `letfun` expect a single expression for their bodies/branches.

But if you want several expressions to be evaluated, you will need to use `do`.

Do takes a variadic amount of expressions and evaluates them one-by-one in order, returning the result of the last one.

### Example
```lisp
(println 
  (do
    "I will be ignored"
    "I will be ignored as well"
    "I will be printed"))
```

## Exceptions
Nomad uses an exception-based error handling model.

### Try
To handle a function that may throw an exception for whatever reason, use `try`.

#### Example
```lisp
(let x "123abc")
(try
  (string_to_num x)
  (println "Couldn't parse " x " to a number!"))
```

### Throw
You can also throw your own exceptions using `throw`.

#### Example
```lisp
(letfun function_that_expects_a_string (str)
  (if (isstr str)
    (println "All good!")
    (throw "The argument to this function must be a string!")))
```
