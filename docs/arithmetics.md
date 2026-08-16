# Arithmetics

## +
Arithmetic Addition

### Signature
```lisp
(+ <lhs> <rhs>)
```

Works on strings and numbers

### Returns
`number` | `string`

## -
Arithmetic Subtraction

### Signature
```lisp
(- <lhs> <rhs>)
```

Only works on numbers

### Returns
`number`

## *
Arithmetic multiplication

### Signature
```lisp
(* <lhs> <rhs>)
```

Works on numbers

However, it is also possible to multiply strings with numbers.

### Examples
```lisp
(* 9 9)
```

returns `81`

```lisp
(* 2 "Hello ") # (* "Hello " 2) works as well
```

returns `"Hello Hello "`

## /
### Signature
```lisp
(/ <lhs> <rhs>)
```

Only works on Numbers

returns `number`

## mod
Arithmetic modulo

### Signature
```lisp
(mod <lhs> <rhs>)
```

Only works on Numbers

returns `number`

## inc
Increment (+ 1)

### Signature
```lisp
(inc <x>)
```

Only works on Numbers

returns `number`

## dec
Decrement (- 1)

### Signature
```lisp
(dec <x>)
```

Only works on Numbers

returns `number`
