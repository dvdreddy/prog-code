(ns code.gcj-sixteen.qual.prob-d
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)))


(defn main-log
  [k c s]
  ;; (println "reader " k c s)
  (let [min-s (loop [st 1 cur 1 cx 1 res []]
                ;; (println "state " st cur cx res (== cx c))
                (if (== cur k)
                  (conj res st)
                  (if (== cx c)
                    (recur (+ cur 1) (+ cur 1) 1 (conj res st))
                    (recur (+ (* (- st 1) k) cur 1)
                           (+ cur 1) (+ cx 1) res))))]
    (if (<= (count min-s) s)
      (str/join " " min-s)
      "IMPOSSIBLE")))

(defn get-input
  [reader]
  [(.nextInt reader) (.nextInt reader) (.nextInt reader)])

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
