(ns code.gcj-fifteen.one-b.prob-b
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




(defn max-cnt
  [r c empty-vals]
  (cond (and (>= r 0) (>= c 0))
        (max
          (let [vx (+ (* r (int (/ c 2)))
                      (if (even? c) 0 (int (/ (+ r 1) 2))))
                poss2 (+ (- r (int (/ (+ r 1) 2)))
                         (- r (int (/ (+ r (if (even? c) 0 1)) 2)))
                         (if (even? r) c (* 2 (int (/ c 2)))))
                poss3 (+ 1
                         (if (even? c) 0 1)
                         (if (even? r) 0 1)
                         (if (odd? (+ r c)) 0 1))
                rem2 (if (> empty-vals vx)
                       (- empty-vals vx) 0)
                rem3 (if (> empty-vals (+ vx poss2))
                       (- empty-vals (+ vx poss2)) 0)]
            ;;(println "Way1")
            ;;(pp/pprint {:poss1 vx :poss2 poss2 :poss3 poss3
;;                        :rem1 empty-vals :rem2 rem2 :rem3 rem3})
            (assert (>= poss3 rem3))
            (+ (* 4 (min empty-vals vx))
               (* 3 (min rem2 poss2))
               (* 2 (min rem3 poss3))))
          (let [vx (+ (* r (int (/ c 2)))
                      (if (even? c) 0 (int (/ r 2))))
                poss2 (+ (- r (int (/ r 2)))
                         (- r (int (/ (+ r (if (even? c) 1 0)) 2)))
                         (if (even? r) c (* 2 (int (/ (+ 1 c) 2)))))
                poss3 (+ 0
                         (if (even? c) 1 0)
                         (if (even? r) 1 0)
                         (if (odd? (+ r c)) 1 0))
                rem2 (if (> empty-vals vx)
                       (- empty-vals vx) 0)
                rem3 (if (> empty-vals (+ vx poss2))
                       (- empty-vals (+ vx poss2)) 0)]
            ;; (println "Way2")
            ;;(pp/pprint {:poss1 vx :poss2 poss2 :poss3 poss3
                        ;; :rem1 empty-vals :rem2 rem2 :rem3 rem3})
            (assert (>= poss3 rem3))
            (+ (* 4 (min empty-vals vx))
               (* 3 (min rem2 poss2))
               (* 2 (min rem3 poss3)))))
        (and (>= r 0) (< c 0))
        (* 2 empty-vals)
        (and (< r 0) (>= c 0))
        (*  2 empty-vals)
        :else 0))

(defn main-log
  [r c n]
  ;; (pp/pprint vals)
  (if (<= n (maxx r c))
    0
    (let [empty-vals (- (* r c) n)
          maxx-val (max-cnt (- r 2) (- c 2) empty-vals)]
      (- (walls r c) maxx-val))))


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
                  [i r c n])))
         (map (fn [[i r c n]]
                  (println "Processing Case" i)
                  (str "Case #" i ": " (main-log r c n))))
         (str/join "\n")
         (spit output-file))))
