(ns code.gcj-sixteen.r1a.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)
           (java.util TreeSet)))


(defn main-log
  [n seqs]
  (println " N " n )
  (println " SEQS ")
  (pp/pprint seqs)
  (let [sorted-seqs (sort seqs)
        px (loop [rs [] seqs sorted-seqs i 0]
             (if (empty? seqs)
               rs
               (let [fx (first seqs)
                     sx (second seqs)]
                 (println "fx sx " fx sx)
                 (if (and (not (nil? sx))
                          (== (nth fx i) (nth sx i)))
                   (recur (conj rs [:not-fixed i fx sx])
                          (sort-by #(subvec % (+ i 1))
                                   (rest (rest seqs)))
                          (inc i))
                   (recur (conj rs [:fixed i fx :unknown])
                          (sort-by #(subvec % (+ i 1))
                                   (rest seqs))
                          (inc i))))))
        start (filter #(= :fixed (first %)) px)
        idx (second (first start))]
    (println "Star vals " )
    (pp/pprint start)
    (assert (= 1 (count start))
            "More than one unknown")
    (loop [res px proc start]
      (println "new res " res
               "new proc " proc)
      (if (empty? proc)
        (str/join " "  (map #(nth (nth % 2) idx) res))
        (let [[_ cur-id fx sx] (first proc)
              [new-res new-proc]
              (reduce (fn [[nrs prc] elem]
                        (println " elemx " elem)
                        (if (= :fixed (first elem))
                          [nrs prc]
                          (let [[_ elem-id elem-fx elem-sx] elem]
                            (cond
                              (= elem-fx elem-sx)
                              [(assoc nrs
                                 elem-id
                                 [:fixed elem-id elem-fx elem-sx])
                               (conj prc [:fixed elem-id elem-fx elem-sx])]
                              (and (== (nth elem-fx cur-id)
                                       (nth fx elem-id))
                                   (not (== (nth elem-sx cur-id)
                                            (nth fx elem-id)))
                                   (or (= sx :unknown)
                                       (== (nth elem-sx cur-id)
                                           (nth sx elem-id))))
                              [(assoc nrs
                                 elem-id
                                 [:fixed elem-id elem-sx elem-fx])
                               (conj prc
                                     [:fixed elem-id elem-sx elem-fx])]
                              (and (== (nth elem-sx cur-id)
                                       (nth fx elem-id))
                                   (not (== (nth elem-fx cur-id)
                                            (nth fx elem-id)))
                                   (or (= sx :unknown)
                                       (== (nth elem-fx cur-id)
                                           (nth sx elem-id))))
                              [(assoc nrs
                                 elem-id
                                 [:fixed elem-id elem-fx elem-sx])
                               (conj prc
                                     [:fixed elem-id elem-fx elem-sx])]
                              :else [nrs prc]))))
                      [res (rest proc)] res)]
          (recur new-res new-proc))))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [n (sort
         (map (fn [ix]
                (into []
                      (map (fn [jx]
                             (.nextInt reader))
                           (range n))))
              (range (- (* 2 n) 1))))]))

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
