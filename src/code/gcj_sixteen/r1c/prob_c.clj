(ns code.gcj-sixteen.r1c.prob-c
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader Simplex]
           (java.util HashSet)))


(defn main-log
  [j p s k]
  (println "The limit is" k)
  (loop [current-k s
         n-offset 0
         res {}]
    ;; (println current-k n-offset res)
    (if (<= current-k k)
      (let [px (filter identity
                       (for [jx (range 1 (+ j 1))
                             px (range 1 (+ p 1))
                             sx (range 1 (+ s 1))]
                         (if (get res [jx px sx] true)
                           (str jx " " px " " sx))))
            pxx (frequencies (mapcat identity
                                     (filter identity
                                             (for [jx (range 1 (+ j 1))
                                                   px (range 1 (+ p 1))
                                                   sx (range 1 (+ s 1))]
                                               (if (get res [jx px sx] true)
                                                 [[jx px]])))))
            pxxx (frequencies (mapcat identity
                                      (filter identity
                                              (for [jx (range 1 (+ j 1))
                                                    px (range 1 (+ p 1))
                                                    sx (range 1 (+ s 1))]
                                                (if (get res [jx px sx] true)
                                                  [[px sx]])))))
            pyyy (frequencies (mapcat identity
                                      (filter identity
                                              (for [jx (range 1 (+ j 1))
                                                    px (range 1 (+ p 1))
                                                    sx (range 1 (+ s 1))]
                                                (if (get res [jx px sx] true)
                                                  [[jx sx]])))))]
        (println "MAX frequencies"
                 (apply max (vals pxx))
                 (apply max (vals pxxx))
                 (apply max (vals pyyy))
                 )
        (when (or
              (> (apply max (vals pxx)) k)
              (> (apply max (vals pxxx)) k)
              (> (apply max (vals pyyy)) k))
          (println " Error here check")
          (clojure.pprint/pprint res)
          )
        (if (> (apply max (vals (frequencies px))) 1)
          (println "Error due to repetition"))

        ;; (clojure.pprint/pprint pxx)
        ;; (clojure.pprint/pprint pxxx)
        ;; (clojure.pprint/pprint pyyy)
        (str (count px) "\n" (clojure.string/join "\n" px)))
      (recur (- current-k 1)
             (+ n-offset 1)
             (reduce
               (fn [res [jx px sx]]
                 (assert (not (get res [jx px sx])))
                 (assoc res [jx px sx] false))
               res (map (fn [[jx px]]
                         [jx px
                          (if (zero? (mod (+ jx px n-offset) s))
                            s (mod (+ jx px n-offset) s))])
                        (for [jx (range 1 (+ j 1))
                             px (range 1 (+ p 1))]
                          [jx px])))))))

(defn get-input
  [reader]
  [(.nextInt reader) (.nextInt reader) (.nextInt reader) (.nextInt reader)])

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

