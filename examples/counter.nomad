#!/usr/bin/env nomad
(letfun count (start end)
  (if (< end start)
    (println "done!")
    (do 
      ((println start) 
      (count ((+ start 1) end))))))

(count (0 10))
