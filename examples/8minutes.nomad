#!/usr/bin/env nomad
(letfun input_loop (correct_answer prompt)
  (if (= correct_answer (readln prompt))
    (println "Correct!")
    (do 
      ((println "False!\nTry again!")
      ((input_loop (correct_answer prompt)))))))

(input_loop ("8" "How many minutes? "))
