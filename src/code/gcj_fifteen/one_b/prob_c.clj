(ns code.gcj-fifteen.one-b.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn main-log
  [n vals]
  ;; (pp/pprint vals)
  )


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
                  (str "Case # " i ": " (main-log n (vec vals))))))
         (str/join "\n")
         (spit output-file))))
