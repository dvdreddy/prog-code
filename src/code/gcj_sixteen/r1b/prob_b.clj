(ns code.gcj-sixteen.r1b.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)))


(defn find-char
  ([st char]
   (.indexOf st (int char)))
  ([st from char]
   (.indexOf st from (int char))))


(defn comp-sols
  [[c1 j1] [c2 j2]]
  (if (and (= c1 c2) (= j1 j2))
    false
    (let [c1-num (Long/parseLong c1)
          j1-num (Long/parseLong j1)
          c2-num (Long/parseLong c2)
          j2-num (Long/parseLong j2)]
      (cond
        (< (Math/abs (- c1-num j1-num))
           (Math/abs (- c2-num j2-num)))
        true
        (> (Math/abs (- c1-num j1-num))
           (Math/abs (- c2-num j2-num)))
        false
        (< c1-num c2-num)
        true
        (> c1-num c2-num)
        false
        :else (< j1-num j2-num)))))

(defn prev-c
  [chr]
  (if (= chr \0)
    \9
    (char (- (int chr) 1))))

(defn next-c
  [chr]
  (if (= chr \9)
    \0
    (char (+ (int chr) 1))))


(defn get-poss-chars
  [c-char j-char]
  (cond
    (and (not= c-char \?) (not= j-char \?))
    [[c-char j-char]]
    (and (not= c-char \?) (= j-char \?))
    [[c-char c-char] [c-char (prev-c c-char)] [c-char (next-c c-char)]
     [c-char \0] [c-char \9]]
    (and (= c-char \?) (not= j-char \?))
    [[j-char j-char] [(prev-c j-char) j-char] [(next-c j-char) j-char]
     [\0 j-char] [\9 j-char]]
    :else
    [[\0 \1] [\1 \0] [\0 \0] [\0 \9] [\9 \0]]))

(defn main-log
  [c j]
  (let [len (count c)]
    (loop [id 0 res [["" ""]]]
      (pp/pprint res)
      (if (== id len)
        (do
          (str/join " " (first (sort (comparator comp-sols) res))))
        (recur
          (inc id)
          (second
            (reduce
              (fn [[val-up val-list-up] [cur-c cur-j]]
                (reduce
                  (fn [[val val-list] [new-c new-j]]
                    (let [new-c-l (Long/parseLong new-c)
                          new-j-l (Long/parseLong new-j)
                          abs-diff (Math/abs (- new-c-l new-j-l))]
                      (cond
                        (<= -1 (- val abs-diff) 1)
                        [(min val abs-diff)
                         (conj val-list [new-c new-j])]
                        (< (- val abs-diff) -1)
                        [val val-list]
                        :else [abs-diff (hash-set [new-c new-j])])))
                  [val-up val-list-up]
                  (map (fn [[c-char j-char]]
                         [(str cur-c c-char) (str cur-j j-char)])
                       (get-poss-chars
                         (.charAt c id) (.charAt j id)))))
              [Long/MAX_VALUE (hash-set)] res)))))))


(defn get-input
  [reader]
  [(.next reader) (.next reader)])

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
