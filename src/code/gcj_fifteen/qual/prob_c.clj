(ns code.gcj-fifteen.qual.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]))




(def mult-map
  {"1" {"1" "1" "-1" "-1"
        "i" "i" "-i" "-i"
        "j" "j" "-j" "-j"
        "k" "k" "-k" "-k"}
   "-1" {"1" "-1" "-1" "1"
         "i" "-i" "-i" "i"
         "j" "-j" "-j" "j"
         "k" "-k" "-k" "k"}
   "i" {"1" "i" "-1" "-i"
        "i" "-1" "-i" "1"
        "j" "k" "-j" "-k"
        "k" "-j" "-k" "j"}
   "-i" {"1" "-i" "-1" "i"
         "i" "1" "-i" "-1"
         "j" "-k" "-j" "k"
         "k" "j" "-k" "-j"}
   "j" {"1" "j" "-1" "-j"
        "i" "-k" "-i" "k"
        "j" "-1" "-j" "1"
        "k" "i" "-k" "-i"}
   "-j" {"1" "-j" "-1" "j"
         "i" "k" "-i" "-k"
         "j" "1" "-j" "-1"
         "k" "-i" "-k" "i"}
   "k" {"1" "k" "-1" "-k"
        "i" "j" "-i" "-j"
        "j" "-i" "-j" "i"
        "k" "-1" "-k" "1"}
   "-k" {"1" "-k" "-1" "k"
         "i" "-j" "-i" "j"
         "j" "i" "-j" "-i"
         "k" "1" "-k" "-1"}})


(defn mult-raw
  [l-oper r-oper]
  (get-in mult-map [l-oper r-oper]))

(def mult (memoize mult-raw))

(defn get-mid-oper-raw
  [l-oper result]
  (->> mult-map keys
       (filter #(= (mult l-oper %) result))
       first))

(def get-mid-oper (memoize get-mid-oper-raw))

(defn get-cumm-arr
  [gen-str]
  (->> gen-str
       (reduce #(conj %1 (mult (first %1) (str %2)))
               '("1"))
       reverse
       rest
       vec))

(defn check-poss
  [cumm-arr]
  (let [cnt (count cumm-arr)
        check-vals (doall (range 0 cnt))
        i-index (filter #(= (cumm-arr %) "i") check-vals)
        k-index (filter #(= (cumm-arr %) "k") check-vals)]
    ;;(println "check-vals" check-vals)
    ;;(println "I-index" i-index)
    ;;(println "K-index" k-index)
    (< (reduce min Integer/MAX_VALUE i-index)
       (reduce max Integer/MIN_VALUE k-index))))


(defn pow
  [val x]
  (condp = (mod x 4)
    0 "1"
    1 val
    2  (mult val val)
    3 (mult val (mult val val))))

(defn check-j
  [cumm-arr l x check-x]
  (if (== x check-x)
    (= "-1" (cumm-arr (- (* l x) 1)))
    (= "-1" (pow (cumm-arr (- l 1)) x))))

(defn get-check-x
  [x]
  (if (> x 20) 20 x))
  ;;x)


(defn main-log
  [l x vals]
  (let [check-x (get-check-x x)
        cumm-str (->> (range) (map (constantly vals))
                      (take check-x) (apply str))
        cumm-arr (get-cumm-arr cumm-str)]
    ;;(println "Cumm arr" cumm-arr)
    ;;(println "Check J" (check-j cumm-arr l x check-x))
    ;;(println "Check Poss" (check-poss cumm-arr))
    (if (and (check-j cumm-arr l x check-x)
             (check-poss cumm-arr))
      "YES" "NO")))


(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [l (.nextInt reader)
                      x (.nextLong reader)
                      vals (.next reader)]
                  [i l x vals])))
         (pmap (fn [px] (let [[i l x vals] px]
                          (println "Processing Case" i)
                          (str "Case #" i ": "
                               (main-log l x vals)))))
         (str/join "\n")
         (spit output-file))))

