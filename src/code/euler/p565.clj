(ns code.euler.p565
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all]
            [clojure.math.numeric-tower :as math])
  (:import [code.util InputReader]
           (java.util HashMap)
           (java.lang Math)))

(defn prime-sieve
  [max-val]
  (loop [res [2]
         cur 3
         sv-set #{}]
    (if (> cur max-val)
      res
      (if (sv-set cur)
        (recur res (+ cur 2) (disj sv-set cur))
        (recur (conj res cur)
               (+ cur 2)
               (apply conj sv-set
                     (loop [resx []
                            cp (* cur cur)]
                       (if (> cp max-val)
                         resx
                         (recur (conj resx cp)
                                (+ cp cur))))))))))

(def prime-x (prime-sieve 1000000))
(def base-prime-x (into [] (rest (take-while #(< % 12101) prime-x))))
(def base-prime (take 15 prime-x))

(defn big-mult
  [a b modi]
  (try
    (mod (* a b) modi)
    (catch  Exception e
      (long (mod (* (bigint a) (bigint b)) modi)))))

(defn fast-mod-exp
  [n e modi]
  (loop [ex e res 1 base (mod n modi)]
    (if (== ex 0)
      res
      (recur
        (bit-shift-right ex 1)
        (if (even? ex)
          res (big-mult res base modi))
        (big-mult base base modi)))))

(def chk-count-0 (atom 0))
(def chk-count-1 (atom 0))
(def chk-count-2 (atom 0))

(defn reset-counters []
  (reset! chk-count-0 0)
  (reset! chk-count-1 0)
  (reset! chk-count-2 0)
  )

(defn check-prime-det
  [x]
  (swap! chk-count-2 inc)
  (if (even? x)
    false
    (let [maxi  (+ (long (math/sqrt x)) 1)]
      (loop [n 3]
        (if (> n maxi)
          true
          (if (== (mod x n) 0)
            false
            (recur (+ n 2))))))))

(defn check-prime-prob0
  [n]
  (swap! chk-count-0 inc)
  (every? #(not (zero? (mod n %))) base-prime))

(defn check-prime-prob1
  [n]
  (swap! chk-count-1 inc)
  (let [rand-ind (min (- (count base-prime-x) 1)
                      (+ 1000 (rand-int (count base-prime-x))))
        rand-prime (get prime-x rand-ind)]
    (if (and (not (even? n)) (== (math/gcd n rand-prime) 1))
      (let [eul (fast-mod-exp rand-prime (/ (- n 1) 2) n)]
        (if (or (== eul 1) (== eul (- n 1)))
          true false)) false)))


(defn check-prime-prob2
  [n]
  (swap! chk-count-1 inc)
  (let [rand-ind  (rand-int (count base-prime-x))
        rand-prime (get base-prime-x rand-ind)
        [d s] (loop [d (- n 1) s 0]
                (if (even? d)
                  (recur (bit-shift-right d 1) (+ s 1))
                  [d s]))]
    (if (and (not (even? n)) (== (math/gcd n rand-prime) 1))
      (let [eul (fast-mod-exp rand-prime d n)]
        (or (== eul 1)
            (loop [cur 0 v (mod eul n)]
              (if  (== cur s)
                false
                (if (== v (- n 1))
                  true
                  (recur (+ cur 1)
                         (big-mult v v n)))))))
      false)))

(defn check-prime-all
  [n]
  (and
    (check-prime-prob0 n)
    (check-prime-prob2 n)
    (check-prime-prob2 n)
    (check-prime-prob2 n)
    (check-prime-det n)
    ))

(def prog (atom 0))

(defn find-rel-primes
  [maxi]
  (let [seq (int (/ maxi (* 2017 100.0)))]
    (reset-counters)
    (doall
      (filter identity
              (cp/pmap
                (min 18 (- (cp/ncpus) 2))
                (fn [x y]
                  (if (zero? (mod y seq))
                    (println "Prog " (/ y seq) "%"))
                  (if (check-prime-all x)
                    x nil))
                (range (- 2017 1)
                       (+ maxi 1)
                       2017)
                (range))))))

(defn check-other-prim
  [maxi]
  (mapcat
    (fn [n]
      (loop [cur (* n n)
             ind 2 res []]
        (if (> cur maxi)
          res
          (recur
            (* cur n )
            (+ ind 1)
            (if (zero? (mod (/ (- (* cur n) 1)
                               (- n 1))
                            2017))
              (conj res [n ind])
              res)))))
    prime-x))

(defn sumn
  [n]
  (/ (* (+ n 1) n) 2))

(defn sing-val
  [maxi val n]
  (let [pow-val (long (Math/pow val n))
        sumx (long (/ maxi pow-val))
        sub-x (long (/ sumx val))
        ]
    (- (* pow-val (sumn sumx))
       (if (zero? sub-x)
         0 (* pow-val val (sumn sub-x))))))

(defn subx-zero
  [maxi val1 n1 val2 n2]
  (let [pow-val1 (long (Math/pow val1 n1))
        pow-val2 (long (Math/pow val2 n2))
        up-val (* pow-val1 pow-val2)]
    (< up-val maxi)))

(defn doub-val
  [maxi val1 n1 val2 n2]
  (let [pow-val1 (long (Math/pow val1 n1))
        pow-val2 (long (Math/pow val2 n2))
        up-val (* pow-val1 pow-val2)
        sumx (long (/ maxi up-val))
        sub-x1 (long (/ sumx val1))
        sub-x2 (long (/ sumx val2))
        sub-x12 (long (/ sumx (* val1 val2)))]
    (- (* up-val (sumn sumx))
       (if (zero? sub-x1)
         0 (* up-val val1 (sumn sub-x1)))
       (if (zero? sub-x2)
         0 (* up-val val2 (sumn sub-x2)))
       (if (zero? sub-x12)
         0 (* -1 (* up-val val1 val2 (sumn sub-x12)))))))

(def val-max 100000000000)

(defn final-sum
  [all-vals]
  (let [init-sum (reduce
                   +
                   (map #(sing-val
                          val-max
                          (first %)
                          (second %))
                        all-vals))
        sub-pairs (filter
                    #(not (>= (first (first %))
                              (first (second %))))
                    (for [x all-vals
                          y all-vals
                          :while (subx-zero
                                   val-max
                                   (first x)
                                   (second x)
                                   (first y)
                                   (second y))]
                      [x y]))
        sub-sum (reduce
                  +
                  (map #(doub-val
                         val-max
                         (first (first %))
                         (second (first %))
                         (first (second %))
                         (second (second %)))
                       sub-pairs))]
    (- init-sum sub-sum)))


(def sing-prim (read-string (slurp "ios/euler/p565.txt")))
(def dual-val (check-other-prim val-max))

(def all-vals
  (sort
    (fn [[v1 n1] [v2 n2]]
      (< (Math/pow v1 n1) (Math/pow v2 n2)))
  (concat
    (map #(vector % 1) sing-prim)
    dual-val)))


(defn- main
  []
  )
