(ns code.gcj-fifteen.one-a.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))


(defn main-log
  [n vals]
  (let [fv (reduce (fn [r y]
                     (let [prev (:prev r) ret (assoc r :prev y)]
                       (if (> prev y)
                         (assoc ret :cnt (+ (:cnt ret) (- prev y)))
                         ret)))
                   {:prev 0 :cnt 0} vals)
        fv-cnt (:cnt fv)
        sv (reduce (fn [r y]
                     (let [prev (:prev r) ret (assoc r :prev y)]
                       (assoc ret :maxi (max (:maxi ret) (- prev y)))
                       ))
                   {:prev 0 :maxi 0} vals)
        sv-max (:maxi sv)
        sv-res (reduce (fn [r y]
                         (+ r (min y sv-max))) 0
                       (subvec vals 0 (- n 1)))]
    (str fv-cnt " " sv-res)))


(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [n (.nextInt reader)
                      vals (doall (map (fn [x] (.nextInt reader))
                                       (range n)))]
                  (println "Processing Case" i)
                  (str "Case #" i ": "
                       (main-log n (vec vals)))
                  )))
         (str/join "\n")
         (spit output-file))))
