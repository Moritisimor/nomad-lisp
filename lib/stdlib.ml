let stdlib_src = [
  "(letfun not (a) (if a false true))";
  "(letfun and (a b) (if a (if b true false) false))";
  "(letfun or (a b) (if a true (if b true false)))";

  "
  (letfun map (f l)
    (do
      (letfun aux (acc h t)
        (if (isunit t)
          (rev acc)
          (aux (cons (f h) acc) (car t) (cdr t))))
        
      (aux () (car l) (cdr l))))
  ";

  "
  (letfun mapi (f l)
    (do
      (letfun aux (acc h t i)
        (if (isunit t)
          (rev acc)
          (aux (cons (f h i) acc) (car t) (cdr t) (+ i 1))))
          
      (aux () (car l) (cdr l) 0)))
  ";

  "
  (letfun rev (l)
    (do
      (letfun aux (acc h t)
        (if (isunit t)
          acc
          (aux (cons h acc) (car t) (cdr t))))
          
      (aux () (car l) (cdr l))))
  ";

  "
  (letfun len (l)
    (do 
      (letfun aux (acc h t)
        (if (isunit t)
          acc
          (aux (+ acc 1) (car t) (cdr t))))

      (aux 0 (car l) (cdr l))))
  ";

  "
  (letfun foreach (f l)
    (do
      (letfun aux (h t)
        (do
          (if (isunit t)
          unit
          (do
            (f h)
            (aux (car t) (cdr t))))))
          
      (aux (car l) (cdr l))))
  ";

  "
  (letfun foreachi (f l)
    (do
      (letfun aux (h t i)
        (do
          (if (isunit t)
            unit
            (do 
              (f h i)
              (aux (car t) (cdr t) (+ i 1))))))
          
      (aux (car l) (cdr l) 0)))
  ";

  "
  (letfun nth (l idx)
    (do
      (letfun aux (h t i)
        (if (isunit t)
          (throw \"List has no such index\")
          (if (= i 0)
            h
            (aux (car t) (cdr t) (- i 1)))))
            
      (aux (car l) (cdr l) idx)))
  ";

  "
  (letfun nth_unit (l idx)
    (do
      (letfun aux (h t i)
        (if (isunit t)
          unit
          (if (= i 0)
            h
            (aux (car t) (cdr t) (- i 1)))))
            
      (aux (car l) (cdr l) idx)))
  ";
]
