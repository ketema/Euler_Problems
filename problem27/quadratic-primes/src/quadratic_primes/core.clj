(ns quadratic-primes.core
  (:gen-class))

(defn is-prime? [n]
  (if (< n 2)
    false
    (let [limit (int (Math/sqrt n))]
      (loop [i 2]
        (if (> i limit)
          true
          (if (zero? (mod n i))
            false
            (recur (inc i))))))))

(defn primes-up-to [limit]
  (filter is-prime? (range 2 (inc limit))))

(defn count-consecutive-primes [a b]
  (loop [n 0]
    (let [result (+ (* n n) (* a n) b)]
      (if (and (>= result 0) (is-prime? result))
        (recur (inc n))
        n))))

(defn find-best-coefficients [a-limit b-limit]
  (let [a-range (range (- (dec a-limit)) a-limit)
        b-primes (primes-up-to b-limit)]
    (->> (for [a a-range
               b b-primes]
           {:a a :b b :primes (count-consecutive-primes a b)})
         (apply max-key :primes)
         (#(* (:a %) (:b %))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (find-best-coefficients 1000 1000)))
