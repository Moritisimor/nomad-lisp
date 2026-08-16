# Variables

## let
Binds a variable to the current scope

You may shadow a variable in an outer scope, but not one within the same scope

### Signature
```lisp
(let <binding name> <expression>)
```

Throws if a binding with the same name already exists within the same scope

Returns `unit`

## mut
Mutates an existing binding

### Signature
```lisp
(mut <binding name> <new expression>)
```

Throws if there is no binding with the same name within the same scope or an outer scope

Returns `unit`

## scoped
Evaluates an expression within a scope, optionally allowing scope-exclusive variables to be introduced

### Signature
```lisp
(scoped (<variables>) <body>)
```

### Example
```lisp
(scoped 
  ((x 10)
   (y 20))
   
   (+ x y)) # 30
# x and y do not exist here anymore
```
