(ns code.gcj-fifteen.qual.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [com.google.common.collect TreeMultiset]
           [java.util Comparator]))


;; Standard clojure code

;; using a java library Guava for multiset
;; available in github


(defn get-int-compartor
  []
  (reify Comparator
    (compare [_ i1 i2]
      (Integer/compare i2 i1))
    (equals [_ _] false)))

(defn get-tree-set
  [comp]
  (TreeMultiset/create comp))

(defn get-first
  [^TreeMultiset multi-set]
  (.getElement (.firstEntry multi-set)))




(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))
        comp (get-int-compartor)]
    (->> (map (fn [i]
                (let [cnt (.nextInt reader)
                      frs (doall (map (fn [x] (.nextInt reader))
                                      (range cnt)))
                      max-t (reduce max 0 frs)]
                  (println "frs" frs)
                  (loop [itr (int 1)]
                    (if (> itr cur-t)
                      (str "Case #" i ": " cur-t)
                      (let [cur-max (get-first hp)
                            new-child (int (/ cur-max 2))]
                        (.remove hp cur-max)
                        (.add hp new-child)
                        (.add hp (- cur-max new-child))
                        (println "New hp" hp (get-first hp) (min cur-t (+ itr 1 (get-first hp))))
                        (recur (min cur-t (+ itr 1 (get-first hp))) (+ itr 1)))))))
              (range 1 (+ t 1)))
         (str/join "\n")
         (spit output-file))))
