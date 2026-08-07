#!/usr/bin/env nomad
(letfun make_person (name age job) (list name age job))
(letfun get_name (person) (nth person 0))
(letfun get_age (person) (nth person 1))
(letfun get_job (person) (nth person 2))

(letfun print_person_info (person) 
  (do
    (let n (get_name person))
    (let a (get_age person))
    (let j (get_job person))
    (println n " is " a " years old and works as a " j ".")))

(let people 
  (list 
    ((make_person "Max Mustermann" 20 "Electrician")
    (make_person "Erika Mustermann" 21 "Software Engineer")
    (make_person "John Doe" 23 "Plumber")
    (make_person "Jane Doe" 19 "Lumberjack"))))

(foreach print_person_info people)
