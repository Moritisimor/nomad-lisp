# Typechecking

## isunit
Checks if an expression is of type `unit`

### Signature
```lisp
(isunit <expr>)
```

## isnum
Checks if an expression is of type `number`

### Signature
```lisp
(isnum <expr>)
```

## isstr
Checks if an expression is of type `string`

### Signature
```lisp
(isstr <expr>)
```

## islist
Checks if an expression is of type `list`

### Signature
```lisp
(islist <expr>)
```

## isfun
Checks if an expression is of type `function`

### Signature
```lisp
(isfun <expr>)
```

## isnative
Checks if an expression is of type `native function`

### Signature
```lisp
(isnative <expr>)
```

## ismac
Checks if an expression is of type `macro`

### Signature
```lisp
(ismac <expr>)
```

## isrecord
Checks if an expression is of type `record`

### Signature
```lisp
(isrecord <expr>)
```

## typeof
Returns the type of an expression as a string

Unknown is a possible type, though it should only appear when I, the developer, have forgotten to implement something

In that case, please post an issue or a pull request

### Signature
```lisp
(typeof <expr>)
```

### Type names
- string
- number
- bool
- list
- record
- unit
- function
- native
- macro

### Example
```lisp
(typeof "Hello") # "string"
(typeof 21) # "number"
(typeof (1 2 3 4)) # "list"
(typeof (lambda (x) x)) # "function"
```
