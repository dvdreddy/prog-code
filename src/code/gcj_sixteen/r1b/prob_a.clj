(ns code.gcj-sixteen.r1b.prob-a
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (java.util HashSet)))


(def order-to-verify
  [0 8 6 7 3 2 4 5 9 1])

(def func-to-verify
  {0 \Z 8 \G 6 \X 7 \S 3 \H 2 \W 4 \U 5 \F 9 \I 1 \O})


(def funcmap
  {0 "ZERO"
   1 "ONE"
   2 "TWO"
   3 "THREE"
   4 "FOUR"
   5 "FIVE"
   6 "SIX"
   7 "SEVEN"
   8 "EIGHT"
   9 "NINE"
   }
  )


(defn all-zero?
  [mp]
  (empty? (filter #(not (zero? %)) (vals mp))))

(defn main-log
  [val]
  (let [init-counts (reduce (fn [x cur]
                              (if (x cur)
                                (assoc x cur (+ (x cur) 1))
                                (assoc x cur 1)))
                            {} val)]
    ;; (println "Original Counts")
    ;; (pp/pprint init-counts)
    (loop [cnts init-counts res []
           cur-list order-to-verify]
      (if (empty? cur-list)
        (do (assert (all-zero? cnts))
            (apply str (sort res)))
        (let [cur (first cur-list)
              get-cnt (cnts (func-to-verify cur))
              new-cnts
              (if (or (nil? get-cnt) (zero? get-cnt))
                cnts
                (reduce (fn [x y]
                          (assoc x
                            y
                            (- (x y) get-cnt)))
                        cnts (funcmap cur)))]
          ;; (println "NEW CNTs after processing " cur get-cnt)
          ;; (pp/pprint new-cnts)
          (recur new-cnts
                 (if (or (nil? get-cnt) (zero? get-cnt))
                   res
                   (concat res (repeat get-cnt cur)))
                 (rest cur-list)))))))


(defn get-input
  [reader]
  [(.next reader)])

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
