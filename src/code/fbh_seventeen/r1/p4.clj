(ns code.fbh-seventeen.r1.p4
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader Factorial]
           [java.lang Math]))


(def modulo 1000000007)

(defn mod-mult
  [& args]
  (reduce #(mod (* (long %1)
                   (long %2))
                modulo)
          (long 1) args))

(defn mod-sum
  [& args]
  (reduce #(mod (+ (long %1)
                   (long %2))
                modulo)
          (long 0) args))


(defn pow [n p]
  (loop [res 1
         base n
         exp p]
    (if (== exp 0)
      res
      (recur
        (if (odd? exp) (mod (* res base) modulo) res)
        (mod (* base base) modulo)
        (bit-shift-right exp 1)))))


;(time (loop [i (int 1)
;             res (long 1)]
;        (if (== (bit-and i modx) 0)
;          (println i))
;        (let [new-res (long (rem (* res i) modulo))]
;          (aset fact-map i new-res)
;          (if (< i 1000002000)
;              (recur (inc i) new-res)))))

;; (time (dorun (map fact (range 1000000000))))

(defn mem-fact [n] (Factorial/factorial n))

(defn calc
  [x y]
  (mod-mult (mem-fact (+ x y))
            (pow (mem-fact x) (- modulo 2))
            (pow (mem-fact y) (- modulo 2))))

(def mem-calc (memoize calc))

(defn main-log
  [n m wts]
  (if (== n 1)
    m
    (let [tot-rad (* 2 (reduce + 0 wts))]
      (reduce (fn [res [i j]]
                (mod-sum
                  res
                  (let [req-points (- tot-rad (get wts i) (get wts j) -1)
                        extra-points (- m req-points)]
                    (if (< extra-points 0)
                      0
                      (mod-mult 2
                                (mem-fact (- n 2))
                                (mem-calc n extra-points))))))
              0 (for [i (range n)
                      j (range (+ i 1) n)]
                  [i j])))))

(defn get-input [reader]
  (let [n (.nextInt reader)
        m (.nextInt reader)]
    [n m (into [] (for [x (range n)]
                    (.nextInt reader)))]))


(defn -main
  ([]
   (-main :stdout :stdin))
  ([output-file input-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 (let [vals (get-input reader)]
                   (println "Processing Case" i)
                   (str "Case #" i ": " (apply main-log vals)))))
          (str/join "\n")
          (spit-internal output-file)))))
