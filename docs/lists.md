# Lists

## list
Takes a variadic sequence of expressions, evaluates them and stores them in a list which is returned

### Signature
```lisp
(list <expressions...>)
```

## append
Combines two lists into one

### Signature
```lisp
(append <list1> <list2>)
```

### Example
```lisp
(append (list 1 2 3) (list 4 5 6))
# (1 2 3 4 5 6)
```

## cons
Prepends an element to a list

### Signature
```lisp
(cons <element> <list>)
```

### Example
```lisp
(cons 0 (list 1 2 3 4 5))
# (0 1 2 3 4 5)
```

## car
Gets the first element of a list

If the list is empty, `unit` is returned

### Signature
```lisp
(car <list>)
```

### Example
```lisp
(car (list 0 1 2 3 4 5))
# 0
```

## cdr
Gets every element of a list except for the first one

If the list is empty, `unit` is returned

### Signature
```lisp
(cdr <list>)
```

### Example
```lisp
(cdr (list 0 1 2 3 4 5))
# (1 2 3 4 5)
```

## rev
Reverses a list

### Signature
```lisp
(rev <list>)
```

### Example
```lisp
(rev (list 1 2 3 4 5))
# (5 4 3 2 1)
```

## len
Returns the length of a list

### Signature
```lisp
(len <list>)
```

### Example
```lisp
(len (list 1 2 3))
# 3
```

## nth
Returns the element at the nth index

If the list holds no such index, an exception is thrown

### Signature
```lisp
(nth <list> <index>)
```

## nth_unit
Returns the element at the nth index

If the lost holds no such index, unit is returned instead

### Signature
```lisp
(nth_unit <list> <index>)
```

## map
Applies a function to each element of a map, accumulating the results in a list which is returned at the end

### Signature
```lisp
(map <function> <list>)
```

### Example
```lisp
(map (lambda (x) (* x x)) (list 1 2 3 4 5))
# (1 4 9 16 25)
```

## mapi
Same as map, except that the current index is also passed to the function

### Signature
```lisp
(mapi <function> <list>)
```

### Example
```lisp
(mapi (lambda (elem index) (* elem index)) (list 1 2 3 4 5))
# (0 2 6 12 20)
```

## foreach
Similar to map, except that the result of each function application is discarded. Use this for side effects

### Signature
```lisp
(foreach <function> <list>)
```

## foreachi
Like foreach, except that the current index is also passed to the function

Like mapi, the higher order function receives the element itself as the first parameter, and the index as the second

### Signature
```lisp
(foreachi <function> <list>)
```

## range
Gets all elements from one index to another

### Signature
```lisp
(range <start> <end> <list>)
```

### Example
```lisp
(range 1 3 (list 1 2 3 4 5))
# (2 3 4)
```

## filter
Filters elements from a list based on a function

### Signature
```lisp
(filter <function> <list>)
```

### Example
```lisp
(filter (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5 6 7 8 9 10)) # Only even numbers
# (2 4 6 8 10)
```

## foldl
Reduces a list to a single element

The function is applied to each element in the list and an accumulator is carried over along with each function invocation

### Signature
```lisp
(foldl <function> <initial accumulator> <list>)
```

### Example
```lisp
(foldl + 0 (list 1 2 3 4 5))
# This is exactly equivalent to: (+ (+ (+ (+ (+ 0 1) 2) 3) 4) 5)
# 15
```

## begins_with
Returns whether a list begins with another list

### Signature
```lisp
(begins_with <list1> <list2>)
```

### Example
```lisp
(begins_with (list 1 2 3 4 5) (list 1 2 3))
# true
(begins_with (list 1 2 3) (list 1 2 3 4 5))
# false
```

## ends_with
Returns whether a list ends with another list

### Signature
```lisp
(ends_with <list1> <list2>)
```

### Example
```lisp
(ends_with (list 1 2 3 4 5) (list 3 4 5))
# true
(ends_with (list 1 2 3 4 5) (list 5 4 3))
# false
```

## list_init
Initializes a list using a function that is applied to each index

### Signature
```lisp
(list_init <amount of elements> <function>)
```

### Example
```lisp
(list_init 10 (lambda (x) x))
# (0 1 2 3 4 5 6 7 8 9)
(list_init 10 (lambda (x) (+ x 1)))
# (1 2 3 4 5 6 7 8 9 10)
```
