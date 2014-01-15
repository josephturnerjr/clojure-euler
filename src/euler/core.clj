(ns euler.core
  (:gen-class))

(def lazy-fib
    (map first
        (iterate
            (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn is-palindrome?
    [n]
    (def str-n (seq (str n)))
    (= str-n (reverse str-n)))
(defn problem-1
    []
    (defn is-mult? [x]
        (or
            (= (mod x 3) 0)
            (= (mod x 5) 0)))
    (apply + 
        (for [i (range 1000) :when (is-mult? i)] i)
    ))

(defn problem-2
    []
    ; old way
    ;(defn fib
    ;    [a b max]
    ;    (if (< b max)
    ;        (conj (fib b (+ a b) max) b)
    ;        nil
    ;    )
    ;)
    ; (apply + (filter even? (fib 0 1 4000000))))

    ; new hotness
    (apply + (filter even? (take-while #(< % 4000000) lazy-fib))))

(defn problem-3
    []
    (defn digits
        [n]
        (count (str n)))
    (first (first (drop-while
                #(< (digits (second %)) 1000)
                (map vector (range) lazy-fib)))))

(defn divisor?
    [c n]
    (if (== c 1)
        false
        (= 0 (mod n c))))
(defn problem-4
    []
    (defn naive-factor
        [n]
        (if (= n 1)
            nil
            (let [factor (first (drop-while #(not (divisor? % n)) (drop-while #(< % 2) (range))))]
                (conj (naive-factor (/ n factor)) factor))))
    (apply max (naive-factor 600851475143)))

(defn problem-5
    []
    (apply max
        (for [x (range 100 1000) y (range x 1000) :when (is-palindrome? (* x y))]
             (* x y))))
        

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Problem 1:" (time (problem-1)))
  (println "Problem 2:" (time (problem-2)))
  (println "Problem 3:" (time (problem-3)))
  (println "Problem 4:" (time (problem-4)))
  (println "Problem 5:" (time (problem-5)))
)
