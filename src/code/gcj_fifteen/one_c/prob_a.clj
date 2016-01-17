(ns code.gcj-fifteen.one-c.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))




(defn main-log
  [r c w]
  (let [zones (* r (int (/ (+ c (- w 1)) w)))]
    (+ (- zones 1) w)))


(defn get-x
  [reader & args]
  [(.nextInt reader) (.nextInt reader) (.nextInt reader)])

(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [vals (get-x reader)]
                  (println "Processing Case" i)
                  (str "Case #" i ": " (apply main-log vals)))))
         (str/join "\n")
         (spit output-file))))
