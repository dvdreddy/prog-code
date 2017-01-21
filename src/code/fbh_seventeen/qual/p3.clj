(ns code.fbh-seventeen.qual.p3
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all]
            [clojure.core.matrix :as matrix]
            [swiss.arrows :refer [-<>]])
  (:import [code.util InputReader]
           [java.lang Math]))


(defn parse-spell
  [spell]
  (let [r-splits (re-matches
                   #"^(\d+)d(4|6|8|10|12|20)((-|\+)(\d+))?"
                   spell)]
    {:roll-count (Integer/parseInt (get r-splits 1))
     :dice (Integer/parseInt (get r-splits 2))
     :res-mod-fn (if (get r-splits 3)
                   #((case (get r-splits 4) "+" - "-" +)
                      % (Integer/parseInt (get r-splits 5)))
                   identity)}))

(def start-vec
  (into [] (concat [1] (repeat 401 0))))

(defn mat [dice]
  (matrix/matrix
    (loop [row (concat (repeat (- 404 dice) 0.0)
                     (repeat dice (/ 1.0 dice))
                     (repeat 402 0.0))
           res []
           i 0]
      (if (< i 402)
        (recur (conj row 0)
               (->> row (drop 404) (take 402)
                    (into []) (conj res))
               (inc i))
        res))))

(def mem-mat (memoize mat))

(def mem-mult-vec nil)

(defn mult-vec
  [dice roll-times]
  (if (== roll-times 0)
    (matrix/identity-matrix 402)
    (matrix/mmul (mem-mult-vec dice (- roll-times 1))
                 (mem-mat dice))))

(def mem-mult-vec (memoize mult-vec))

;; (time (dorun (pmap #(mem-mult-vec % 20) [4 6 8 10 12 20])))

(defn main-log
  [h spells]
  (->> spells
       (map (fn [spell]
              (let [{:keys [roll-count dice res-mod-fn]}
                    (parse-spell spell)
                    final-res (res-mod-fn h)]
                (cond
                  (<= final-res 0)
                  1.0
                  (<= final-res 400)
                  (-<> dice (mem-mult-vec roll-count)
                       (matrix/mmul start-vec)
                       (drop final-res <>)
                       (reduce + 0.0 <>))
                  :else 0.0))))
       (apply max) (format "%.6f")))


(defn get-input [reader]
  (let [h (.nextInt reader)
        s (.nextInt reader)]
    [h (into [] (sort (for [x (range s)]
                        (.next reader))))]))


(defn -main
  ([]
   (println "starting")
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
