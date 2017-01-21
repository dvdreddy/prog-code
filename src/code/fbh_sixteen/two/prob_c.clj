(ns code.fbh-sixteen.two.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util TreeSet)))


(def modx 1000000007)

(defn mx-ht-ind
  [tree-set cur-ind]
  ;; (println "cur-ind " cur-ind)
  (let [vx (sort (map first cur-ind))]
    (loop [vxx (rest vx)
           old (first vx)
           px 1
           sums (first vx)
           sqrs (mod (* sums sums) modx)
           res 0]
      (if (empty? vxx)
        (do (doall (map #(.add tree-set %) vx))
            res)
        (let [cur (first vxx)
              new-sqr (mod (* cur cur) modx)
              new-sqr-tot  (mod (+ sqrs new-sqr) modx)
              break (not (.isEmpty (.subSet tree-set old cur)))]
          ;; (println "CX " res cur new-sqr new-sqr-tot break sums)
          (recur (rest vxx)
                 cur
                 (if break 1 (+ px 1))
                 (if break cur (mod (+ sums cur) modx))
                 (if break new-sqr new-sqr-tot)
                 (if break
                   res
                   (mod (+ modx (- (mod (+ res new-sqr-tot
                                           (mod (* (- px 1) new-sqr) modx))
                                        modx)
                                   (mod (* 2 (mod (* cur sums) modx)) modx)))
                        modx))))))))

(defn main-log
  [& ladder-vecs]
  (let [n (count ladder-vecs)
        same-heights (group-by second ladder-vecs)
        rev-height-sort (sort (comp (fn [a b]
                                      (if (> (key a) (key b)) -1 1)))
                              same-heights)
        sub-tree-set (TreeSet.)]
    (reduce (fn [res x]
              (mod  (+ res (mx-ht-ind sub-tree-set (val x)))
                    modx)) 0 rev-height-sort)))

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

