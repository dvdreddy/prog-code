(ns code.fbh-seventeen.qual.p1
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [java.lang Math]))


(def pi Math/PI)

(defn translate
  [[x y]]
  [(- x 50.0) (- y 50.0)])

(defn rotate
  [[x y]]
  [y (* -1.0 x)])

(defn tranform
  [pt]
  (rotate (translate pt)))

(defn radius-check
  [[x y]]
  (< (+ (* x x) (* y y))
     (* 50.0 50.0)))

(defn prog-val
  [[x y]]
  (let [atan2-val (* -1.0 (Math/atan2 y x))
        adj-ang (if (> atan2-val 0)
                  atan2-val
                  (+ (* 2.0 pi) atan2-val))]
    (* (/ adj-ang (* 2 pi)) 100.0)))


(defn main-log
  [p x y]
  (let [pt (tranform [x y])]
    (if (and (radius-check pt)
             (< (prog-val pt) p))
      "black" "white")))


(defn get-input [reader]
  [(.nextInt reader) (.nextInt reader) (.nextInt reader)])


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
