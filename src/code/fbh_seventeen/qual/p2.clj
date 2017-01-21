(ns code.fbh-seventeen.qual.p2
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [java.lang Math]))

(defn main-log
  [n wts]
  (println "N is " n)
  (pp/pprint wts)
  (loop [i 0
         j (- n 1)
         trips 0]
    (if (> i j)
      (if (== i (+ j 1))
        trips (- trips 1))
      (recur
        (+ i (Math/ceil (/ 50 (get wts j))) -1.0)
        (dec j)
        (inc trips)))))


(defn get-input [reader]
  (let [n (.nextInt reader)]
    [n (into [] (sort (for [x (range n)]
                        (.nextInt reader))))]))


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
