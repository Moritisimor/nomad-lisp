# Conversions

## string_to_num
Converts a string to a number

### Signature
```lisp
(string_to_num <string>)
```

Throws if the string couldn't be parsed

Returns `number`

## to_string
Converts an expression to a string

### Signature
```lisp
(to_string <expression>)
```

### Example
```lisp
(to_string 123)
# "123"
(to_string (list 1 2 3))
# "(1 2 3)"
(to_string (lambda (x) x))
# "<FUNCTION>"
(to_string if)
# "<NATIVEFUNCTION>"
```

Returns `string`
