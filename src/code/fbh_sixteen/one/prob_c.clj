(ns code.fbh-sixteen.one.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashMap)))



(defn sub-log
  [x costs]
  (reduce (fn [[cnt val sum] cost]
            (let [vx (min x (+ sum cost))
                  sub-cnt (- vx sum)
                  new-cnt (+ cnt sub-cnt)
                  new-val (if (== 0 new-cnt)
                            0.0
                            (/ (+ (* cnt val)
                                  (* sub-cnt (/ (- vx sum) 2.0)))
                               new-cnt))
                  new-sum (+ sum cost)]
              (if (< x new-sum)
                (reduced [new-cnt new-val])
                [new-cnt new-val new-sum])))
          [0 0.0 0] costs))


(defn main-log
  [a b costs]
  (let [cost-sum (reduce + costs)
        [ccnt cval] (sub-log cost-sum costs)
        [acnt aval] (sub-log (mod a cost-sum) costs)
        [bcnt bval] (sub-log (mod b cost-sum) costs)]
    ;;(println "cost-sum " cost-sum "ccnt " ccnt "cval " cval
    ;; "acnt " acnt "aval " aval "bcnt " bcnt "bval " bval)
    (format
      "%.8f"
      (double
        (/ (- (+ (* cval (* ccnt (int (/ b cost-sum))))
                 (* bval bcnt))
              (+ (* cval (* ccnt (int (/ a cost-sum))))
                 (* aval acnt)))
           (- (+ bcnt (* ccnt (int (/ b cost-sum))))
              (+ acnt (* ccnt (int (/ a cost-sum))))))))))


(defn get-input
  [reader]
  (let [n (.nextInt reader)]
    [(.nextInt reader) (.nextInt reader)
     (doall (for [i (range n)] (.nextInt reader)))]))


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
