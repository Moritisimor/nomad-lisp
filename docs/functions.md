# Functions

## letfun
Mainly syntactical sugar for binding a variable to a lambda

### Signature
```lisp
(letfun (<parameters...>) <body>)
```

The parameters are not curried

Nomad functions simply know how many arguments they expect

Invoking a function with the wrong amount of arguments will throw an exception

## lambda
Creates an invokable object (a function)

### Signature
```lisp
(lambda (<parameters...>) <body>)
```

You can do anything with this function, bind it to a variable, invoke it immediately, pass it around to other functions etc.
