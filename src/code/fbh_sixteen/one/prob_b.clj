(ns code.fbh-sixteen.one.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashMap)
           (com.google.common.collect MinMaxPriorityQueue MinMaxPriorityQueue$Builder)))


(defn create-min-max-pq
  ([size]
   (.create (MinMaxPriorityQueue/expectedSize size)))
  ([size comp]
   (.create (.expectedSize (MinMaxPriorityQueue/orderedBy comp)
                           size))))



(defn main-log
  [l n m d w]
  (let [^MinMaxPriorityQueue laundry
        (create-min-max-pq n (comparator (fn [x y] (< (first x) (first y)))))
        ^MinMaxPriorityQueue drier (create-min-max-pq n)]
    (dorun (map #(.add laundry [%1 %2]) w (range n)))
    (loop [lx 0 tx 0]
      ;; (println " lx val is " lx tx " main-log " l n m d
      ;;          " drier size " (.size drier) " laundry size " (.size laundry))
      (if (== lx l)
        tx
        (let [drier-peek (.peek drier)
              laundry-peek (max tx (first (.peek laundry)))]
          (if (and (not (nil? drier-peek))
                   (or (<= drier-peek laundry-peek)
                       (>= (.size drier) (min d l))
                       (> (+ lx (.size drier)) l)))
            (recur (+ lx 1) (max (.poll drier) tx))
            (let [[cur-time cur-ind] (.poll laundry)]
              (.add drier (+ (max tx cur-time) d))
              (.add laundry [(+ cur-time (get w cur-ind))
                             cur-ind])
              (recur lx (max tx cur-time)))))))))


(defn get-input
  [reader]
  (let [l (.nextInt reader)
        n (.nextInt reader)]
    [l n (.nextInt reader) (.nextInt reader)
     (into [] (doall (for [i (range n)] (.nextInt reader))))]))


(defn -main
  ([]
   (-main :stdin :stdout))
  ([input-file output-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 (let [vals (get-input reader)
                       res (apply main-log vals)]
                   (println "Processing Case" i res)
                   (str "Case #" i ": " res))))
          (str/join "\n")
          (spit-internal output-file)))))
