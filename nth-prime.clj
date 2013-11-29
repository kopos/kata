(defn is-divisible? [a b]
  (= (rem a b) 0))

(defn prime? [n primes]
  (not
    (empty?
      (filter false?
        (for [p primes] (is-divisible? n p))))))

(defn nth-prime [n]
  (loop [2 n] (
      (var primes [2])
      (var guess 3)
      (true?
        (prime? guess primes)
        (def primes (conj primes guess))))))

(nth (primes 5) 5)
