# Records
Record types are a nice feature for collecting several pieces of data in a single expression without having to use bare indexes like with lists

## record
Constructs a record

### Signature
```lisp
(record (<key value pairs...>))
```

### Example
```lisp
(record 
  (name "John Doe")
  (age 21) 
  (job "Electrician"))
```

## .
Gets the expression behind a record's field

### Signature
```lisp
(. <record> <field name>)
```

### Example
```lisp
(let person 
  (record
    (name "John Doe")
    (age 21)
    (job "Electrician")))

(println (. person name)) # "John Doe"
(println (. person age)) # 21
(println (. person job)) # "Electrician"
```

## record_mut
Mutates a field of a record

### Signature
```lisp
(record_mut <record> <field name> <new value>)
```

### Example
```lisp
(let person 
  (record 
    (name "John Doe") 
    (age 21) 
    (job "Electrician")))

(record_mut person age (inc (. person age)))
(println (. person age)) # 22
```
