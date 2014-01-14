(ns euler.core
  (:gen-class))

(defn problem-1
    []
    (defn is-mult? [x]
        (or
            (= (mod x 3) 0)
            (= (mod x 5) 0)))
    (apply + 
        (for [i (range 1000) :when (is-mult? i)] i)
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Problem 1:" (problem-1)))
