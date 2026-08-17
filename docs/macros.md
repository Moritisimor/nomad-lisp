# Macros
Nomad Lisp supports macros!

The macro system is very small, but it works well enough for most situations

## Defining Macros
The way to define macros is using the builtin function `letmac`

### Signature
```lisp
(letmac <name> (<arguments...>) <expressions...>)
```

### Example
```lisp
(letmac unless (cond yes no) if cond no yes)
```

To invoke it:
```lisp
(unless false 
  (println "I will be printed!") 
  (println "I will not be printed!"))
```

This example would expand into this:
```lisp
(if false
  (println "I will not be printed!")
  (println "I will be printed!"))
```

This macro is already in the standard library though, so no need to define it yourself every time
