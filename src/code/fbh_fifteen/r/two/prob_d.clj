(ns code.fbh-fifteen.r.two.prob-d
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashMap)))



(defn nearest-two-pow
  [n]
  (loop [m n res 1]
    (if (== m 0)
      (bit-shift-left res 1)
      (recur (bit-shift-right m 1) (bit-shift-right res 1)))))



(defn get-range-tree
  [k n depth]
  ()
  )



(defn main-log
  [k items]
  (let [final-map (trie-calc items)]
    (final-map k)))



(defn get-input
  [reader]
  (let [n (.nextInt reader)
        k (.nextInt reader)]
    [k (doall (for [i (range n)] (.next reader)))]))


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

