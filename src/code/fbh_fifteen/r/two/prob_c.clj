(ns code.fbh-fifteen.r.two.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashMap)))



(defn merge-children-vals
  [child-maps]
  ;;(println "merge child maps" child-maps)
  (reduce (fn [cur v]
            ;; (println "reduce loop" cur v)
            (let [final-mp (HashMap.)]
              (dorun
                (for [x cur y v]
                  (do
                    (let [sum (+ (key x) (key y))
                          val-sum (+ (val x) (val y))]
                      ;; (println " For loop " x y sum val-sum (get final-mp sum))
                      (if (not (and (get final-mp sum)
                                    (< (get final-mp sum) val-sum)))
                        (do (.put final-mp sum val-sum)
                            ;; (println "added " sum val-sum (get final-mp sum))
                            ))))))
              (into {} final-mp))) child-maps))


(defn trie-calc
  [word-list]
  ;; (println " word list " word-list)
  (if (= 1 (count word-list))
    {0 0 1 1}
    (let [sub-fn (fn [entry] (->> entry val trie-calc))
          sub-child (group-by first word-list)]
      (if (= 1 (count sub-child))
        (->> sub-child first val (map rest) trie-calc
             (reduce-kv #(assoc %1 %2 (if (== 0 %2) 0 (+ 2 %3))) {}))
        (->> sub-child (map sub-fn) merge-children-vals)))))


(defn main-log
  [k items]
  (let [final-map (trie-calc items)]
    (final-map k)))


(defn get-input
  [reader]
  (let [n (.nextInt reader)
        k (.nextInt reader)]
    [k (doall (for [i (range n)] (.next reader)))]))


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

