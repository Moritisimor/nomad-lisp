#!/usr/bin/env nomad
(letfun is_adult (age) (> age 17))
(let ages (quote (11 20 56 12 30 54 21 17)))
(foreach (lambda (a)
  (unless (is_adult a)
    unit
    (println "A person of age " a " is an adult!")))
    
  ages)
