(ns code.fbh-seventeen.r1.p1
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [swiss.arrows :refer [-<>>]]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [java.lang Math]))

(defn norm
  [n m pies]
  (if (<= m n)
    (into [] (concat  [0] pies (repeat (- n m) Integer/MAX_VALUE)))
    (update (into [] (concat [0] (take n pies)))
            (- n 1) #(apply min % (drop n pies)))))

(defn main-log
  [n m pies]
  (last (reduce
          (fn [cur-p i]
            (let [prices (get pies i)]
              ;; (println "CRP  " cur-p prices )
              (reduce (fn [cur-p-main [x y]]
                        ;(println "UPT "
                        ;         x y
                        ;         (get cur-p-main  (min (+ x y 1) n))
                        ;         (+ (get cur-p x)
                        ;            (get prices y)))
                        (update
                          cur-p-main (min (+ x y 1) n)
                          #(let [tar (+ (get cur-p x)
                                        (get prices y))]
                             (if (< % tar) % tar))))
                      cur-p
                      (for [x (range i n)
                            y (range m)]
                        [x y]))))
          (norm n m (first pies))
          (range 1 n))))


(defn get-input [reader]
  (let [n (.nextInt reader)
        m (.nextInt reader)]
    [n m (mapv (fn [_]
                 (let [prices (->> (range m)
                                   (map (fn [_] (.nextInt reader)))
                                   sort (into []))]
                   (first
                     (reduce (fn [[res prev] i]
                               [(conj res (+ prev (get prices i)
                                             (* (+ i 1) (+ i 1))))
                                (+ prev (get prices i))])
                             [[] 0] (range m)))))
               (range n))]))


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
