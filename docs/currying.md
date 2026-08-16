# Currying
Nomad functions are not curried by default, however, currying is very much possible

## Examples
```lisp
(let add
  (lambda (x)
    (lambda (y)
      (+ x y))))

(let add10 (add 10))
(println (add10 20)) # 30
```
