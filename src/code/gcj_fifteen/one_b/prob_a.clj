(ns code.gcj-fifteen.one-b.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn reverse-number [n]
  (Integer. (clojure.string/reverse (str n))))


(declare mez)

(defn non-mez
  [n]
  ;; (println non-mez n)
  (if (= n 0)
    0
    (if (<= n (reverse-number n))
      (+ 1 (mez (- n 1)))
      (if (= (mod n 10) 0)
        (+ 1 (mez (- n 1)))
        (+ 1 (min (mez (- n 1)) (mez (reverse-number n))))))))

(def mez (memoize non-mez))

(dorun (map mez (range 1000000)))

(defn get-half-diff
  [cnt]
  ()

  )


(def main-arr
  (reduce (fn [mn cur]
            )



          [10 1]))

(defn main-log
  [n]
  ;; (pp/pprint vals)
  (mez n))



(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [n (.nextInt reader)]
                  (println "Processing Case" i)
                  (str "Case #" i ": " (main-log n)))))
         (str/join "\n")
         (spit output-file))))
