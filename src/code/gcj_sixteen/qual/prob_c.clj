(ns code.gcj-sixteen.qual.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet HashMap)))



(def prime-list [      2      3      5      7     11     13     17     19     23     29
                 31     37     41     43     47     53     59     61     67     71
                 73     79     83     89     97    101    103    107    109    113
                 127    131    137    139    149    151    157    163    167    173
                 179    181    191    193    197    199    211    223    227    229
                 233    239    241    251    257    263    269    271    277    281
                 283    293    307    311    313    317    331    337    347    349])


(def prim-list-range (range (count prime-list)))

(def maxi (bit-shift-left 1 14))

(def mods (make-array Integer/TYPE
                      maxi 9
                      (count prime-list)))


(defn update-cache []
  (dorun (map (fn [prim-index prim-val]
                (dorun (map
                         #(aset mods  1 (- % 2) prim-index
                                (int (mod % prim-val)))
                         (range 2 11))))
              prim-list-range prime-list))
  (dorun (map (fn [cur-val]
                (println "processing val " cur-val)
                (let [prev-val (bit-shift-right cur-val 1)
                      prev-array (aget mods prev-val)]
                  (dorun (pmap
                           (fn [base]
                             (let [cur-array (aget prev-array (-  base 2))]
                               (dorun (map
                                        (fn [prim-index prim-val]
                                          (aset mods cur-val (- base 2) prim-index
                                                (int (mod (* base (+ (aget cur-array prim-index)
                                                                     (bit-and cur-val 1)))
                                                          prim-val))))
                                        prim-list-range prime-list))))
                           (range 2 11)))))
              (range 2 maxi))))


(defn get-mod
  [n prim pow]
  (if (== 0 pow)
    1
    (mod (* n (get-mod
                n prim (- pow 1))) prim)))


(defn get-val
  [dig v]
  (str "1" (apply str (map #(if (zero? (bit-and v (bit-shift-left 1 %)))
                             0 1) (range (- dig 3) -1 -1)))  "1"))

(defn main-log
  [dig lim]
  (let [mod-arr (make-array Integer/TYPE (count prime-list) 9)]
    (dorun (map (fn [prim-index prim-val]
                  (dorun (map
                           #(aset mod-arr prim-index (- % 2)
                                  (int (mod (+ 1 (get-mod % prim-val (- dig 1)))
                                            prim-val)))
                           (range 2 11))))
                prim-list-range prime-list))
    ;; (println "Mod Arr")
    ;; (clojure.pprint/pprint  mod-arr)
     ;; (println "Mods ")
    ;; (clojure.pprint/pprint mods)
    (->> (range 1 (min maxi (bit-shift-left 1 (- dig 2))))
         (map (fn [val]
                [val (filter #(not (nil? %))
                             (map
                               (fn [base]
                                 (let [x (first (filter
                                                  #(== 0 (mod (+ (aget mod-arr % (- base 2))
                                                                 (aget mods val (- base 2) %))
                                                              (get prime-list %)))

                                                  prim-list-range))]
                                   (if x (get prime-list x) nil)))
                               (range 2 11)))]))
         (filter #(== 9 (count (second %))))
         (take lim)
         (map #(str (get-val dig (first %)) " " (str/join " " (second %))))
         (str/join "\n"))))


(defn get-input
  [reader]
  [(.nextInt reader) (.nextInt reader)])

(defn -main
  ([]
   (println "Initialization")
   (-main :stdin :stdout))
  ([input-file output-file]
   (delete-if-exists output-file)
   (update-cache)
   (println "getting input")
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 [i (get-input reader)]))
          (map (fn [[ix vx]]
                 ;; (println "Processing Case" ix)
                 [ix (apply main-log vx)]))
          (map (fn [[ix rx]]
                 (str "Case #" ix ":\n" rx)))
          (str/join "\n")
          (spit-internal output-file)))))
