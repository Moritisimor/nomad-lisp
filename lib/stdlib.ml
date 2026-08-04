let stdlib_src = [
  "(letfun not (a) (if a false true))";
  "(letfun inc (i) (+ i 1))";
  "(letfun dec (i) (- i 1))";

  "
  (letfun foldl (f acc l)
    (do
      (letfun aux (a h t)
        (if (isunit t)
          a
          (aux (f a h) (car t) (cdr t))))
      
    (aux acc (car l) (cdr l))))
  ";

  "
  (letfun begins_with (l1 l2)
    (if (< (len l1) (len l2))
      false
      (do
        (letfun aux (l1h l1t l2h l2t)
          (if (isunit l2t)
            true
            (if (= l1h l2h)
              (aux (car l1t) (cdr l1t) (car l2t) (cdr l2t))
              false)))
                
        (aux (car l1) (cdr l1) (car l2) (cdr l2)))))
  ";
  
  "(letfun ends_with (l1 l2) (begins_with (rev l1) (rev l2)))";
  "(letfun has_prefix (s1 s2) (begins_with (chars s1) (chars s2)))";
  "(letfun has_suffix (s1 s2) (ends_with (chars s1) (chars s2)))";

  "
  (letfun list_init (n f)
    (do
      (letfun aux (acc i)
        (if (< i 0)
          acc
          (aux (cons (f i) acc) (dec i))))
          
      (aux () (dec n))))
  ";

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
          (aux (inc acc) (car t) (cdr t))))

      (aux 0 (car l) (cdr l))))
  ";

  "(letfun strlen (s) (len (chars s)))";

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
            (aux (car t) (cdr t) (dec i)))))
            
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
