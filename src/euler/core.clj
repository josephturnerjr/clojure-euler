(ns euler.core
  (:gen-class)
  (:use clojure.set))

(def lazy-fib
    (map first
        (iterate
            (fn [[a b]] [b (+ a b)]) [0N 1N])))


(defn is-palindrome?
    [n]
    (def str-n (seq (str n)))
    (= str-n (reverse str-n)))

(defn divisor?
    [c n]
    (if (== c 1)
        false
        (= 0 (mod n c))))

(defn any-divisor?
    [coll n]
    (some #(divisor? % n) coll))

(def lazy-primes
    (map #(last (first %))
        (iterate
            (fn [[primes coll]]
                [
                    (conj primes (first coll))
                    (drop-while
                        #(any-divisor? primes %)
                        (rest coll))
                ]) 
            [[] (drop 2 (range))])))

(def lazy-primes-so
    (map #(first %)
        (drop 1
            (iterate
                (fn [[prime coll]]
                    [
                        (first coll)
                        (filter
                            #(not (divisor? (first coll) %))
                            (rest coll))
                    ]) 
                [2 (drop 3 (range))]))))

(defn sieve
    [maximum]
    (def candidates (apply sorted-set (range 3 maximum 2)))
    (defn get-primes
        [candidates primes]
        (if (empty? candidates)
            primes
            (let [prime (first candidates)]
                (recur (difference candidates (set (range prime maximum prime))) (conj primes prime)))))
    (get-primes candidates [2]))
    

(defn naive-factor
    [n]
    (if (= n 1)
        nil
        (let [factor (first (drop-while #(not (divisor? % n)) (drop-while #(< % 2) (range))))]
            (conj (naive-factor (/ n factor)) factor))))

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

(defn problem-25
    []
    (defn digits
        [n]
        (count (str n)))
    (first (first (drop-while
                #(< (digits (second %)) 1000)
                (map vector (range) lazy-fib)))))

(defn problem-3
    []
    (apply max (naive-factor 600851475143)))

(defn problem-4
    []
    (apply max
        (for [x (range 100 1000) y (range x 1000) :when (is-palindrome? (* x y))]
             (* x y))))

(defn problem-5
    []
    (defn lcm
        [numbers]
        (if (empty? numbers)
            nil
            (let [current (first numbers)]
            (conj (lcm (map #(if (divisor? current %) (/ % current) %)
                                    (filter #(not= 1 %)
                                            (rest numbers)))) current)
            )))
            
    (apply * (lcm (range 2 20))))
(defn pow
    [x p]
    (reduce * (repeat p x)))
(defn square
    [x]
    (pow x 2))
(defn sum
    [coll]
    (apply + coll))

(defn problem-6
    []
    (- (square (sum (range 101)))
       (sum (map square (range 101)))))

(defn problem-7
    []
    (nth lazy-primes 10000))

(defn problem-8
    []
    (def digs (seq "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")) 
    (defn get-digits
        [digs]
        (if (< (count digs) 5)
            nil
            (cons (map #(Integer/parseInt (str %)) (take 5 digs)) (get-digits (rest digs)))))
    (apply max (map #(apply * %) (get-digits digs))))

(defn problem-10
    []
    (sum (sieve 2000000)))

(defn -main
  [& args]
  ;(println "Problem 1:" (time (problem-1)))
  ;(println "Problem 2:" (time (problem-2)))
  ;(println "Problem 3:" (time (problem-3)))
  ;(println "Problem 4:" (time (problem-4)))
  ;(println "Problem 5:" (time (problem-5)))
  ;(println "Problem 6:" (time (problem-6)))
  ;(println "Problem 7:" (time (problem-7)))
  ;(println "Problem 8:" (time (problem-8)))
  (println "Problem 10:" (time (problem-10)))
  ;(println "Problem 25:" (time (problem-25)))
)
