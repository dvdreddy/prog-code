(ns code.gcj-fifteen.r2.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [clojure.set :as clj-set]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader Simplex]
           (java.util HashSet)))


(defn main-log
  [sent]
  (let [x (mapcat (fn [sen id]
                    (let [wrds (.split sen " ")]
                      (mapv #(vector % id) wrds))) sent (range))
        grp-by (group-by first x)
        frnch-words (set (.split (first sent) " "))
        eng-words (set (.split (second sent) " "))
        common-wrds (clj-set/intersection frnch-words eng-words)
        frnch-only (clj-set/difference frnch-words common-wrds)
        eng-only (clj-set/difference eng-words common-wrds)
        other-wrds (clj-set/difference
                     (keys grp-by) (clj-set/union frnch-words eng-words))]
    (loop [res {}
           all-wrds (clj-set/union frnch-only eng-only)
           proc {}]
      (if (empty? all-wrds)
        ;; matching code

        (loop [cur-wrd [(first all-wrds)]])

        )
      )


    )
  )


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    (mapv (fn [x] (.nextLine reader)) (range n))))

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

