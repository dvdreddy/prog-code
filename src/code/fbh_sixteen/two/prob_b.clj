(ns code.fbh-sixteen.two.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util ArrayList)))

(def ncr)

(defn ncr-all
  [n r]
  (let [x (if (or (== n r) (== r 0))
            (double 1.0)
            (if (or (== r 1) (== r (- n 1)))
              (double n)
              (+ (ncr (- n 1) r)
                 (ncr (- n 1) (- r 1)))))]
    ;; (println "Asking ncr-all" n r x)
    x))


(def ncr (memoize ncr-all))

(defn log-pow
  [n k]
 ;; (println " Math pow " (Math/pow n k))
  (Math/pow n k))


(defn dist
  [n k p]
  (loop [cur 2
         res [1.0 (if (= k 1) (- 1.0 p) 1.0)]]
    ;; (println "Cur val is " cur)
    (if (> cur n)
      res
      (recur
        (+ cur 1)
        (conj res
              (- (get res (- cur 1))
                 (if (< cur k)
                   (double 0.0)
                   (let [y (* (ncr (- cur 1) (- k 1))
                              (log-pow (double p) k)
                              (log-pow (- (double 1.0) p) (- cur k)))]
                     (println " Cur val is " cur (ncr (- cur 1) (- k 1))
                              (log-pow (double p) k) (log-pow (- (double 1.0) p) (- cur k))
                              )
                     y))))))))

(defn main-log
  [n k p]
  (let [dist-vec (dist n k p)
        val-lst (ArrayList. (+ 1 n))]

    (println "Dist vec" dist-vec)
    (doall (map (fn [x] (.add val-lst 0.0)) (range (+ 1 n))))
    (doall (map (fn [x]
                  ;; (println "Processing " x)
                  (let [cur-p (.get val-lst (int x))]
                    (doall (map (fn [y]
                                  (let [v (.get val-lst (int y))
                                        vx (+ cur-p (- 1.0 (get dist-vec (- y x))))]
                     ;;                (println " vvx " x y v vx)
                                    (if (>= vx v)
                                      (.set val-lst (int y) vx))))
                                (range (+ x 1) (+ n 1))))))
                (range n)))
    (format "%.9f" (.get val-lst (int n)))))

(defn get-input
  [^InputReader reader]
  [(.nextInt reader) (.nextInt reader) (.nextDouble reader)])

(defn -main
  ([]
   (-main :stdin :stdout))
  ([input-file output-file]
   (delete-if-exists output-file)
   (doall (for [i (range 3001)
                j (range i)]
            (ncr i j)))
   (println (ncr 3000 2664))
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 (let [vals (get-input reader)]
                   (println "Processing Case" i)
                   (str "Case #" i ": " (apply main-log vals)))))
          (str/join "\n")
          (spit-internal output-file)))))

