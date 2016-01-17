(ns code.gcj-fifteen.one-a.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn two-points
  [[x1 y1] [x2 y2] vals n]
  (if (and (== x1 x2) (== y1 y2))
    (- n 1)
    (let [mp (->> vals
                  (map (fn [[x3 y3]]
                         (- (* (- y3 y1) (- x2 x1))
                            (* (- y2 y1) (- x3 x1)))))
                  (filter (complement zero?))
                  (group-by neg?))]
      ;; (pp/pprint mp)
      (min (count (mp false)) (count (mp true))))))



(defn point-vals
  [pt vals n]
  (->> vals
       (map #(two-points pt % vals n))
       (reduce min)))


(defn main-log
  [n vals]
  ;; (pp/pprint vals)
  (str "\n" (str/join "\n" (map #(point-vals % vals n) vals))))


(defn get-x
  [reader & args]
  [(.nextInt reader) (.nextInt reader)])

(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [n (.nextInt reader)
                      vals (doall (map (partial get-x reader) (range n)))]
                  (println "Processing Case" i)
                  (str "Case # " i ":" (main-log n (vec vals))))))
         (str/join "\n")
         (spit output-file))))
