(ns code.gcj-sixteen.r1a.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)
           (java.util TreeSet)))



(defn main-log
  [n edg]
  (->> (range 1 (+ n 1))
       (map (fn [st]
              (loop [prev #{st} alls [st] cur st last -1]
                (if (contains? prev (edg cur))
                  (do (println "existing " prev alls (edg cur))
                    (cond (== st (edg cur))
                          (count prev)
                          (== last (edg cur))
                          (if (> (count
                                (filter #(== (val %) cur) edg)

                                )

                          :else -1)
                      ))
                  (recur (conj prev (edg cur))
                         (conj alls (edg cur))
                         (edg cur) cur)))))
       (reduce max -1)))



(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [n (reduce #(assoc %1 (+ %2 1) (.nextInt reader))
               {} (range n))]))


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

