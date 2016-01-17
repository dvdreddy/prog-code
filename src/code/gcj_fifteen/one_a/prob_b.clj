(ns code.gcj-fifteen.one-a.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
             [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn start-range
  [b-seq t-time]
  (reduce (fn [[d p] q]
            [(long (+ d (/ (+ t-time (- q 1)) q)))
             (if (= (mod t-time q) 0) (+ p 1) p)])
          [0 0] b-seq))


(defn get-time
  [b n b-seq]
  (loop [lo 0 hi (long (/ Long/MAX_VALUE 10000))]
    ;; (println "lo hi " lo hi)
    (assert (<= lo hi))
    (let [mid (long (/ (+ lo hi) 2))
          [d p] (start-range b-seq mid)]
      (assert (<= p b))
      ;; (println "d p " d p)
      (cond
        (and (> n d) (<= n (+ d p)))
        mid
        (<= n d)
        (recur lo (- mid 1))
        :else (recur (+ mid 1) hi)))))


(defn main-log
  [b n vals]
  ;; (pp/pprint vals)
  (let [t (get-time b n vals)
        [d p] (start-range vals t)
        v-vec (vec vals)
        bb (filter #(= 0 (mod t (v-vec (- % 1))))
                   (range 1 (+ b 1)))]
    (nth bb (- n (+ d 1)))))


(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [b (.nextInt reader)
                      n (.nextInt reader)
                      vals (doall (map (fn [x] (.nextInt reader))
                                       (range b)))]
                  (println "Processing Case" i)
                  (str "Case #" i ": "
                       (main-log b n (vec vals)))
                  )))
         (str/join "\n")
         (spit output-file))))
