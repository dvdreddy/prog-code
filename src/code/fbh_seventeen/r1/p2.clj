(ns code.fbh-seventeen.r1.p2
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [swiss.arrows :refer [-<>>]]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [java.lang Math]))


(defn sq-cache
  [sqs points r]
  (reduce
    (fn [mp [x y :as sq]]
      (let [left y right (+ y r)
            down x up (+ x r)]
        (->> points
             (filter #(and (<= left (second %) right)
                           (<= down (first %) up)))
             count (assoc mp sq))))
    {} sqs))

(defn intersect
  [r [x1 y1] [x2 y2] ]
  (and  (or (<= x1 x2 (+ x1 r))
            (<= x2 x1 (+ x2 r)))
        (or (<= y1 y2 (+ y1 r))
            (<= y2 y1 (+ y2 r)))))


(defn main-log
  [r points]
  (let [sqs (for [x (distinct (map first points))
                  y (distinct (map second points))]
              [x y])
        sq-map (sq-cache sqs points r)]
    (->> (for [sq1 sqs sq2 sqs] [sq1 sq2])
         (filter #(not (apply intersect r %)))
         (map #(+ (sq-map (first %))
                  (sq-map (second %))))
         (concat (vals sq-map))
         (apply max))))


(defn get-input [reader]
  (let [n (.nextInt reader)
        r (.nextInt reader)]
    [r (for [_ (range n)]
         [(.nextInt reader) (.nextInt reader)])]))


(defn -main
  ([]
   (-main :stdout :stdin))
  ([output-file input-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 (let [vals (get-input reader)]
                   (println "Processing Case" i)
                   (str "Case #" i ": " (apply main-log vals)))))
          (str/join "\n")
          (spit-internal output-file)))))

