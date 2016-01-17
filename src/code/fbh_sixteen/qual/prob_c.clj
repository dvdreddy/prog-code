(ns code.fbh-sixteen.qual.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))



(defn main-log
  [p vals]
  (let [[res _ cnt _] (reduce (fn [[res stk cnt sum] val]
                                (loop [resx res
                                       stkx stk
                                       cntx (+ cnt 1)
                                       sumx (+ sum val)]
                                  ;; (println "VAL" val " resx " resx " stkx " stkx " sumx " sumx)
                                  (if (<= sumx p)
                                    [resx stkx cntx sumx]
                                    (recur (+ resx (- cntx 1))
                                           (+ 1 stkx) (- cntx 1) (- sumx (get vals stkx))))))
                              [0 0 0 0] vals)]
    (+ res (/ (* cnt (+ cnt 1)) 2))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [(.nextInt reader)
     (into [] (for [i (range n)]
                (.nextInt reader)))]))


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
