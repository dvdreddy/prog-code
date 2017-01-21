(ns code.gcj-sixteen.r1b.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader Simplex]
           (java.util HashSet)))


(defn main-log
  [word-list]
  (let [n (count word-list)
        first-words (into [] (set (map first word-list)))
        second-words (into [] (set (map second word-list)))
        first-word-size (count first-words)
        m (+ (count first-words) (count second-words))
        c-arr (make-array Double/TYPE n)
        b-arr (make-array Double/TYPE m)
        res-arr (make-array Double/TYPE n)
        a-arr (make-array Double/TYPE m n)]
    (dorun (map (fn [y x]
                  (dorun (map (fn [yy xx]
                                (if (= (first xx) x)
                                  (aset a-arr y yy -1.0)))
                              (range) word-list)))
                (range) first-words))
    (dorun (map (fn [y x]
                  (dorun (map (fn [yy xx]
                                (if (= (second xx) x)
                                  (aset a-arr (+ first-word-size y) yy -1.0)))
                              (range) word-list)))
                (range) second-words))
    (dorun (map (fn [x]
                  (aset b-arr x -1.0)) (range m)))
    (dorun (map (fn [x]
                  (aset c-arr x -1.0)) (range n)))
    ;; (println "C-Arr")
    ;; (pp/pprint c-arr)
    ;; (println "B-arr")
    ;; (pp/pprint b-arr)
    ;; (println "A- ARR")
    ;; (pp/pprint a-arr)
    (let [simplex (Simplex. a-arr b-arr c-arr)
          res-val (.solve simplex res-arr)]
      ;; (println "RES ARR")
      ;; (pp/pprint res-arr)
      (+ n (int res-val)))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [(mapv (fn [x]
           [(.next reader)
            (.next reader)])
           (range n))]))

(defn -main
  ([]
   (println "Initialization")
   (-main :stdin :stdout))
  ([input-file output-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 [i (get-input reader)]))
          (map (fn [[ix vx]]
                 (println "Processing Case" ix)
                 [ix (apply main-log vx)]))
          (map (fn [[ix rx]]
                 (str "Case #" ix ": " rx)))
          (str/join "\n")
          (spit-internal output-file)))))
