(ns code.gcj-sixteen.r2.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)))

(def exp
  {"P" "R"
   "S" "P"
   "R" "S"
   }
  )

(def mem-get-bst)

(defn get-bst
  [strt n]
  (if (== n 0)
    strt
    (let [px (mem-get-bst strt (- n 1))
          qx (mem-get-bst (exp strt) (- n 1))
          res1 (str px qx)
          res2 (str qx px)
          ]
      (if (<= (compare res1 res2) 0)
        res1 res2))))

(def mem-get-bst (memoize get-bst))


(defn gen-winner
  [n winner]
  (loop [round-x 1
         cur-str winner]
    (if (> round-x n)
      cur-str
      (recur (+ round-x 1)
             (apply str
                    (map (fn [char-x]
                           (if (= \P char-x)
                             (if (< (- n round-x) 3)
                               "PR" "RP")
                             (if (= \S char-x)
                               (if (< (- n round-x) 2)
                                 "PS" "SP")
                               (if (or (== round-x n)
                                       (>= (- n round-x) 4))
                                 "RS" "SR"))))
                         cur-str))))))

(def gen-all-p)

(defn gen-all-p
  [n]
  (if (== n 1)
    ["R" "P" "S"]
    (concat (map #(str "R" %) (gen-all-p (- n 1)))
            (map #(str "P" %) (gen-all-p (- n 1)))
            (map #(str "S" %) (gen-all-p (- n 1))))))


(defn check-no-ties
  [line-up]
  ;; (println "Checking no ties " line-up)
  (loop [cur line-up err (some #(= (first %) (second %))
                               (partition 2 line-up))]
    (println "Curr r " cur "ERR " err)
    (if (or err (== 1 (count cur)))
      (if err false true)
      (let [pairs (partition 2 cur)
            new-x (map (fn [x]
                         (let [px (sort x)]
                           (cond
                             (and (= (first px) \P)
                                  (= (second px) \R)) \P
                             (and (= (first px) \R)
                                  (= (second px) \S)) \R
                             (and (= (first px) \P)
                                  (= (second px) \S)) \S
                             :else (do
                                     (println "WTF " px)
                                     (throw (RuntimeException. "Error "))))))
                       pairs)
            some-x (some #(= (first %) (second %))
                         (partition 2 new-x))]
        (recur new-x some-x)))))

(defn main-log-2
  [n r p s]
  (let [pow-n (bit-shift-left 1 n)
        eligib (filter (fn [x]
                         (let [grp-by (group-by identity x)]
                           (and (== r (count (grp-by \R)))
                                (== p (count (grp-by \P)))
                                (== s (count (grp-by \S))))))
                       (filter check-no-ties (gen-all-p pow-n)))]
    (println "elibgs ")
    (clojure.pprint/pprint eligib)
    (if (== 0 (count eligib))
      "IMPOSSIBLE"
      (first (sort eligib)))))



(defn main-log
  [n r p s]
  (let [all-winners [(mem-get-bst "P" n)
                     (mem-get-bst "R" n)
                     (mem-get-bst "S" n)]
        eligib (filter (fn [x]
                         (let [grp-by (group-by identity x)]
                           (and (== r (count (grp-by \R)))
                                (== p (count (grp-by \P)))
                                (== s (count (grp-by \S))))))
                       all-winners)]
    (println "elibgs ")
    (clojure.pprint/pprint eligib)
    (if (== 0 (count eligib))
      "IMPOSSIBLE"
      (first (sort eligib)))))

(defn get-input
  [reader]
  [(.nextInt reader) (.nextInt reader) (.nextInt reader) (.nextInt reader)])

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

