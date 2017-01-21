(ns code.fbh-sixteen.two.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))

(defn get-best
  [right-vec equal-v cur-ind]
  (loop [cur (- cur-ind equal-v)
         past (if (> cur 0) (get right-vec (- cur 1)) -1)
         res 0]
    (if (== cur cur-ind)
      res
      (recur (+ cur 1)
             (get right-vec cur)
             (if (= past (get right-vec cur))
               res (+ res 1))))))



(defn best-job
  [n left-init right-init]
  (assert (= (count left-init) (count right-init))
          "Size mismatch")
  (let [left-vec (into [] left-init)
        right-vec (into [] right-init)
        max-ind n]
    (loop [cur 1
           equal-v (if (= (first left-vec) (first right-vec)) 1 0)
           res [(bit-xor equal-v 1)]]
      (if (>= cur max-ind)
        res
        (recur (+ cur 1)
               (if (= (get left-vec cur)
                      (get right-vec cur))
                 (+ equal-v 1) 0)
               (conj res (if (= (get right-vec cur)
                                (get left-vec cur))
                           (get res (- cur 1))
                           (+ (get res (- cur 1))
                              (get-best right-vec equal-v cur)
                              (if (= (get right-vec cur)
                                     (get right-vec (- cur 1)))
                                0 1))
                           )))))))

(defn get-val
  [val-vec cur-ind]
  (if (== cur-ind 0)
    0
    (get val-vec (- cur-ind 1))))

(defn main-log
  [n left right]
  (let [jack-vec (best-job n left right)
        jill-vec (best-job n (reverse left) (reverse right))]
    ;; (println "JACK " jack-vec)
    ;; (println "JILL " jill-vec)
    (loop [res (+ n 1)
           cur 0]
      (if (> cur n)
        res
        (do
          ;; (println "Cur x " cur (- n cur) (get-val jack-vec  cur) (get-val jill-vec (- n cur)))
          (recur (min res
                      (max (get-val jack-vec  cur)
                           (get-val jill-vec (- n cur))))
               (+ cur 1)))))))

(defn get-input
  [reader]
  [(.nextInt reader) (.next reader) (.next reader)])

(defn -main
  ([]
   (-main :stdin :stdout))
  ([input-file output-file]
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

