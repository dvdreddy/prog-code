(ns code.fbh-sixteen.one.prob-d
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader BipartiteMatching]
           (java.util HashMap)))

(def two-pow
  #{1 2 4 8 16})

(def start-map
  {1 1
   2 1
   4 2
   8 4
   16 8})

(def rank-map
  {1 1
   2 2
   4 3
   8 5})

(defn win-i-j?
  [win-mat i j n]
  (not (zero? (get win-mat (+ (* i n) j)))))

(def >> bit-shift-right)
(def << bit-shift-left)

(defn get-active-full
  [n]
  (loop [nx n idx 0 resx []]
    (if (== nx 0)
      resx
      (recur (>> nx 1) (+ idx 1)
             (if (zero? (bit-and nx 1))
               resx (conj resx idx))))))

(def get-active (memoize get-active-full))

(defn get-ones
  [n]
  (loop [cnt (int 0) nx (int n)]
    (if (== nx 0)
      cnt
      (recur (+ cnt (bit-and nx 1))
             (>> nx 1)))))

(defn get-count-map
  [n]
  (reduce #(let [one-count (get-ones %2)]
            (if (two-pow one-count)
              (if-let [cur-v (get %1 one-count)]
                (assoc %1 one-count
                          (conj cur-v %2))
                (assoc %1 one-count [%2]))
              %1))
          {} (range 1 (+ n 1))))

(def count-map (get-count-map (<< 1 16)))


(defn worst-log
  [win-mat ind n]
  (if (every? identity
              (map #(or (== ind %)
                        (win-i-j? win-mat ind % n))
                   (range n)))
    1 (+ (/ n 2) 1)))

(defn is-poss?
  [i j n win-mat]
  (assert (== (bit-and i j) 0)
          "Should be disjoint")
  (let [active-i (get-active i)
        active-j (get-active j)]
    ;; (println "Active " i j active-i active-j)
    (assert (== (count active-i)
                (count active-j))
            (str "Same size should be paired"
                 " " active-i " " active-j))
    (let [resx (->>
                 (mapv #(mapv (fn [jx]
                                (if (win-i-j? win-mat %1 jx n)
                                  1 0)) active-j) active-i)
                 (BipartiteMatching.)
                 (.run)
                 (== (count active-i)))]
      ;; (println "is-poss?" i j n resx)
      resx)))


(defn start-poss-map
  [n win-mat cnt-map]
  (assert (= 1 (get-ones n))
          "N not a power of two")
  (let [start-count (start-map n)
        all-bit-n (- (<< 1 n) 1)]
    ;; (println "all-bit-n" all-bit-n)
    (apply hash-map
           (mapcat
             #(vector %1 (is-poss? %1 (bit-xor all-bit-n %1)
                                   n win-mat))
             (cnt-map start-count)))))

(defn get-poss-map-index
  [index n win-mat poss-map cnt-map]
  (apply hash-map
         (mapcat (fn [val]
                   [val (some #(and (not= val %)
                                    (zero? (bit-and val %))
                                    (poss-map (bit-or val %))
                                    (is-poss? val % n win-mat))
                              (cnt-map index))])
                   (cnt-map index))))

(defn get-cnt-map
  [n]
  (apply hash-map
         (mapcat (fn [entry]
                   [(key entry)
                    (filterv
                      #(< (apply max (get-active %)) n)
                      (val entry))])
                 count-map)))


(defn get-poss-map
  [n win-mat]
  (let [cnt-map (get-cnt-map n)]
    ;; (println "New count map" n cnt-map)
    (loop [index (>> (start-map n) 1)
           poss-map (start-poss-map n win-mat cnt-map)]
      ;; (println "Index " index " Poss map " poss-map)
      (if (== index 0)
        poss-map
        (recur (>> index 1)
               (merge poss-map (get-poss-map-index
                                 index n win-mat poss-map
                                 cnt-map)))))))

(defn best-log
  [n win-mat]
  (let [poss-vals (map key (filter #(val %)
                                   (get-poss-map n win-mat)))]
    (map
      (fn [ind]
        (let [ind-pow (<< 1 ind)]
          (reduce (fn [res val]
                    (if (zero? (bit-and val ind-pow))
                      res
                      (min res (rank-map (count (get-active val))))))
                  (+ (/ n 2) 1) poss-vals)))
      (range n))))


(defn main-log
  [n win-mat]
  (if (= n 1)
    "\n1 1"
    (let [worst-places (mapv #(worst-log win-mat % n) (range n))
          best-places (best-log n win-mat)]
      (str "\n"
           (str/join "\n"
             (map #(str %1 " " %2) best-places worst-places))))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [n (doall (into []
                    (for [i (range n)
                          j (range n)]
                      (.nextInt reader))))]))


(defn -main
  ([]
   (-main :stdin :stdout))
  ([input-file output-file]
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
