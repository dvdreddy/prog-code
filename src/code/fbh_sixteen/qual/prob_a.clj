(ns code.fbh-sixteen.qual.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))

(defn nc2
  [n]
  (if (< n 2) 0 (/ (* n (- n 1)) 2)))

(defn dist
  [[x1 y1] [x2 y2]]
  (let [v1 (- x1 x2)
        v2 (- y1 y2)]
    (+ (* v1 v1) (* v2 v2))))

(defn main-log
  [& all-vals]
  (->> all-vals
       (map (fn [x] (map #(dist x %) all-vals)))
       (map #(vals (frequencies %)))
       (map (fn [x] (reduce #(+ %1 (nc2 %2)) 0 x)))
       (reduce +)))

(defn get-input
  [reader]
  (into [] (let [n (.nextInt reader)]
             (for [x (range n)]
               [(.nextInt reader) (.nextInt reader)]))))

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
