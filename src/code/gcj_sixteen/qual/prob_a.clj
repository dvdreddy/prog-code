(ns code.gcj-sixteen.qual.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)))


(defn main-log
  [val]
  (if (zero? val)
    "INSOMNIA"
    (loop [st (HashSet.)
           vx val]
      (if (== (.size st) 10)
        (- vx val)
        (do (dorun (map #(.add st (String/valueOf %))
                        (.toCharArray(String/valueOf vx))))
            ;; (println "vals " vx  (.size st))
            (recur st (+ vx val)))))))

(defn get-input
  [reader]
  [(.nextInt reader)])

(def check-all
  (let [call-count (atom 0)]
    (proxy
      [InputReader] []
      (nextInt
        ([]
         (do (swap! call-count inc)
             (if (== @call-count 1)
               1000000
               (- @call-count 2)
               )))))))

(defn -main
  ([]
   (println "Initialization")
   (-main check-all :stdout))
  ([input-file output-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 [i (get-input reader)]))
          (map (fn [[ix vx]]
                 ;; (println "Processing Case" ix)
                 [ix (apply main-log vx)]))
          (map (fn [[ix rx]]
                 (str "Case #" ix ": " rx)))
          (str/join "\n")
          (spit-internal output-file)))))
