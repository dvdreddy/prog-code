(ns code.prac.2k14.r1b.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (nth sorted halfway)))

(defn abs [n] (max n (- n)))

(defn one-char
  [n & args]
  (if (= 1 (count (distinct (map first args))))
    (let [vals (map count args)
          med (median vals)
          mp (map #(abs (- med %)) vals)]
      (reduce + mp))
    Integer/MIN_VALUE))


(defn main-log
  [n vals]
  ;; (pp/pprint vals)
  (let [vx (map #(partition-by identity %) vals)
        dist-cnt (count (distinct (map count vx)))
        cnt (apply map (partial one-char n) vx)
        valx (reduce + cnt)]
    (if (and (= 1 dist-cnt) (>= valx 0))
      valx
      "Fegla Won")))

(defn get-x
  [reader & args]
  (.next reader))

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
                  (str "Case #" i ": " (main-log n (vec vals))))))
         (str/join "\n")
         (spit output-file))))
