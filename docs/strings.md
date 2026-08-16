# Strings

## splitws
Splits a string by whitespace

### Signature
```lisp
(splitws <string>)
```

## sprint
Turns one or more expressions into a single string

### Signature
```lisp
(sprint <expressions>)
```

### Example
```lisp
(let my_list (list 1 2 3 4))
(let my_number 21)
(sprint "my_list = " my_list ", my_number = " my_number)
# "my_list = (1 2 3 4), my_number = 21"
```


### Example
```lisp
(splitws "  Nomad   Lisp  is   Written in    OCaml!   ")
# ("Nomad" "Lisp" "is" "Written" "in" "OCaml!")
```

## chars
Turns a string into a list of characters

### Signature
```lisp
(chars <string>)
```

### Example
```lisp
(chars "Hello")
# ("H" "e" "l" "l" "o")
```

## lower
Lowercases a string

### Signature
```lisp
(lower <string>)
```

### Example
```lisp
(lower "HELLO WORLD!")
# "hello world!"
```

## trim
Cuts leading and trailing whitespace off of a string

### Signature
```lisp
(trim "    Hello   World!   ")
# "Hello   World!"
```

## has_prefix
Returns whether a string has a prefix (begins with another string)

### Signature
```lisp
(has_prefix <string> <substring>)
```

### Example
```lisp
(has_prefix "Hello World!" "Hello")
# true
```

## has_suffix
Returns whether a string has a suffix (ends with another string)

### Signature
```lisp
(has_suffix <string> <substring>)
```

### Example
```lisp
(has_suffix "Hello World!" "World!")
# true
```
