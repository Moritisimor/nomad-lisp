# Conditionals

## not
Negates a boolean. If true false, if false true

### Signature
```lisp
(not <boolean>)
```

## or
Returns true if either of the two expressions is true. Short-circuits when the first expression is true

### Signature
```lisp
(or <boolean expression a> <boolean expression b>)
```

## and
Returns true if both expressions are true. Short-circuits when the first expression is false

### Signature
```lisp
(and <boolean expression a> <boolean expression b>)
```

## =
Equality checking

### Signature
```lisp
(= <x> <y>)
```

## !=
Inverse of `=`

### Signature
```lisp
(!= <x> <y>)
```

## >
Greater

### Signature
```lisp
(> <lhs> <rhs>)
```

Checks whether `lhs` is bigger than `rhs`

Only works on numbers

Returns `bool`

## <
Smaller than

### Signature
```lisp
(< <lhs> <rhs>)
```

Checks whether `lhs` is smaller than `rhs`

Only works on numbers

Returns `bool`

## >=
Greater or equal

### Signature
```lisp
(>= <lhs> <rhs>)
```

Checks whether `lhs` is greater than or equal to `rhs`

Only works on numbers

Returns `bool`

## <=
Smaller or equal

### Signature
```lisp
(<= <lhs> <rhs>)
```

Checks whether `lhs` is smaller than or equal to `rhs`

Only works on numbers

Returns `bool`

## if
If-branching

### Signature
```lisp
(if <cond> <yes> <no>)
```

If `cond` evaluates to `true`, `yes` is evaluated

If `cond` evaluates to `false`, `no` is evaluated

If `cond` is not a `bool`, an exception is thrown

## unless
The inverse of `if`

### Signature
```lisp
(unless <cond> <no> <yes>)
```

## when
Basically `if` but without the no branch

Really just used for imperative control flow

### Signature
```lisp
(when <cond> <body>)
```

## switch
Basically like `switch` out of C# or Java

### Signature
```lisp
(switch <scrutinee> (<arms...>))
```

### Example
```lisp
(let scrutinee 100)
(switch scrutinee
  (10 (println "This is 10!"))
  (50 (println "This is 50!"))
  (100 (println "This is 100!"))
  (_ (println "This is neither 10, 50 or 100!")))
```

Mechanically similar to if, except that there are several branches instead of just yes or no

The cases are allowed to be arbitrary expressions

The `_` case is special because as soon as this case is encountered, its corresponding arm is evaluated, so basically a `default`-case


