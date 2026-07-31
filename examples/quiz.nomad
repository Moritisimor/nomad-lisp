#!/usr/bin/env nomad
(letfun input_loop (correct_answer prompt)
  (if (= correct_answer (readln prompt))
    (println "Correct!")
    (do 
      (println "False!\nTry again!")
      ((input_loop correct_answer prompt)))))

(input_loop "berlin" "What is the capital of Germany? ")
(input_loop "21" "What's 9 + 10? ")
(input_loop "ocaml" "What language is Nomad-LISP written in? ")
(println "All questions answered correctly!")
