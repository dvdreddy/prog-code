(ns code.gcj-fifteen.qual.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))


;; Standard clojure code

(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (map (fn [i]
                (let [cnt (.nextInt reader)
                      frs (.next reader)]
                  (->> frs
                       (map #(- (int %) (int \0)))
                       (reduce #(conj %1 (+ (first %1) %2)) '(0))
                       reverse rest
                       (map #(- (+ %1 1) %2) (range))
                       (reduce max 0)
                       (str "Case #" i ": "))))
              (range 1 (+ t 1)))
         (str/join "\n")
         (spit output-file))))
