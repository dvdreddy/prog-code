(ns code.fbh-sixteen.one.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashMap)))

(def maxi (Integer/MAX_VALUE))

(defn fk
  [a]
  (case (mod a 4)
    0 a
    1 (+ a 3)
    2 (+ a 2)
    3 (+ a 1)))


(defn main-log
  [ques]
  (let [first-q (first ques)]
    (loop [a 0
           b (if (> first-q 1) 1 maxi)
           c (if (> first-q 2) 2 maxi)
           d (if (> first-q 3) 3 maxi)
           prev-ques first-q
           q (rest ques)]
      (if (empty? q)
        (min
          (if (< prev-ques 98) (+ a 3) maxi)
          (if (< prev-ques 99) (+ b 2) maxi)
          (if (< prev-ques 100) (+ c 1) maxi)
          d)
        (let [nxt-ques (first q)
              diff (- nxt-ques prev-ques)]
          (println "A B C D " a b c d prev-ques nxt-ques diff)
          (recur
            d
            (min
              (if (or (<= diff 0) (> diff 10))
                (if (and (< prev-ques 98) (> nxt-ques 1)) (+ a 4) maxi) a)
              (if (and (< prev-ques 99) (> nxt-ques 1)) (+ b 3) maxi)
              (if (and (< prev-ques 100) (> nxt-ques 1)) (+ c 2) maxi)
              (if (> nxt-ques 1) (+ d 1) maxi))
            (min
              (if (or (<= diff 1) (> diff 20))
                (if (and (< prev-ques 98) (> nxt-ques 2)) (+ a 5) maxi) (+ a 1))
              (if (or (<= diff 0) (> diff 10))
                (if (and (< prev-ques 99) (> nxt-ques 2)) (+ b 4) maxi) b)
              (if (and (< prev-ques 100) (> nxt-ques 2)) (+ c 3) maxi)
              (if (> nxt-ques 2) (+ d 2) maxi))
            (min
              (if (or (<= diff 2) (> diff 30))
                (if (and (< prev-ques 98) (> nxt-ques 3)) (+ a 6) maxi) (+ a 2))
              (if (or (<= diff 1) (> diff 20))
                (if (and (< prev-ques 99) (> nxt-ques 3)) (+ b 5) maxi) (+ b 1))
              (if (or (<= diff 0) (> diff 10))
                (if (and (< prev-ques 100) (> nxt-ques 3)) (+ c 4) maxi) c)
              (if (> nxt-ques 3) (+ d 3) maxi))
            nxt-ques
            (rest q)))))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [(doall (for [i (range n)] (.nextInt reader)))]))


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
