(ns code.random.rfi-subseq
  (:require [swiss.arrows :refer :all]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.java.io :refer [writer]])
  (:import [java.util.Random]
           (java.util Random)))


(defn get-smallest-subseq
  [x k]
  (let [n (count x)
        [cur-seq rest-seq] (split-at (- n (- k 1)) x)]
    (loop [rest-seq rest-seq
           cur-seq cur-seq
           result []
           cnt-map (->> cur-seq
                        frequencies
                        (mapcat identity)
                        (apply sorted-map))]
      ;; (pp/pprint cnt-map)
      ;; (pp/pprint result)
      (if (== k (count result))
        (apply str result)
        (let [new-val (key (first cnt-map))
              new-cnt-map (-<>> cur-seq
                                (take-while #(not= new-val %))
                                (conj <> new-val)
                                (reduce #(let [vx (update %1 %2 - 1)]
                                          (if (zero? (get vx %2))
                                            (dissoc vx %2) vx))
                                        cnt-map)
                                (update <> (first rest-seq)
                                        (fnil inc 0)))]
          (recur
            (rest rest-seq)
            (-<>> cur-seq (drop-while #(not= new-val %))
                  rest (concat <> [(first rest-seq)]))
            (conj result new-val)
            new-cnt-map))))))

(def seed 56)

(def random (Random. seed))

(def char-seqs (map char (range 97 123)))

(defn random-char []
  (nth char-seqs (.nextInt random (count char-seqs))))

(defn random-string [len]
  (apply str (take len (repeatedly random-char))))



(defn generate
  [ts-count [low high] file-suff]
  (let [string-gen (fn [] (random-string
                            (+ low (.nextInt random (- high low)))))
        in (writer (str "/tmp/rfi-subseqs/input_" file-suff ".txt"))
        out (writer (str "/tmp/rfi-subseqs/ouput_" file-suff ".txt"))]
    (.write in (str ts-count "\n"))
    (dorun
      (map #(let [k (inc (.nextInt random (count %1)))]
             (.write in (str %1 "\n")) (.write in (str k "\n"))
             (.write out (str (get-smallest-subseq %1 k) "\n"))
             (println "Done with " %2))
           (repeatedly ts-count string-gen) (range)))
    (.close in)
    (.close out)))
