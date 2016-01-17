(ns code.gcj-fifteen.one-b.prob-b-two
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))


(defn shitx
  [r c]
  (if (even? c)
    (* r (int (/ c 2)))
    (+ (* r (int (/ c 2))) (int (/ (+ r 1) 2)) )))

(defn maxx
  [r c]
  (if (or (<= r 0) (<= c 0))
    0
    (max (shitx r c ) (shitx c r))))


(defn walls
  [r c]
  (+ (* (- c 1) r) (* c (- r 1))))



(defn get-r-c
  [vecx r c [pr pc]]
  (if (= 1 (get-in vecx [pr pc]))
    (let [pc1 (+ pc 1)
          pcx1 (- pc 1)
          pr1 (+ pr 1)
          prx1  (- pr 1)]
      (+
        (if (< pc1 c) (get-in vecx [pr pc1]) 0)
        (if (>= pcx1 0) (get-in vecx [pr pcx1]) 0)
        (if (< pr1 r) (get-in vecx [pr1 pc]) 0)
        (if (>= prx1 0) (get-in vecx [prx1 pc]) 0)))
    0))

(defn get-count
  [p-val r c]
  (let [gx (loop [x [] v p-val cnt (* r c)]
             (if (= cnt 0)
               x
               (recur (conj x (bit-and v 1))
                      (bit-shift-right v 1)
                      (- cnt 1))))
        grid (vec (map vec (partition c gx)))
        ;; nil-val (pp/pprint grid)
        n-cnt (count ((group-by identity gx) 1))
        nbh (mapcat (fn [rx]
                      (map #(vector rx %) (range c)))
                    (range r))
        vals (->> nbh
                  (map #(get-r-c grid r c %))
                  (reduce +))]
    ;; (println "vals" nbh)
    (assert (even? vals))
    [n-cnt (/ vals 2)]))


(defn main-log
  [r c n]
  ;; (pp/pprint vals)
  (let [all-poss (bit-shift-left 1 (* r c))
        vals (->> (range all-poss)
                  (map #(get-count % r c))
                  (filter #(= n (first %)))
                  (map second)
                  (reduce min))]
    vals))


(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [r (.nextInt reader)
                      c (.nextInt reader)
                      n (.nextInt reader)]
                  (println "Processing Case" i)
                  (str "Case #" i ": " (main-log r c n)))))
         (str/join "\n")
         (spit output-file))))

