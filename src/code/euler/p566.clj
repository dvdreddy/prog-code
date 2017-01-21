(ns code.euler.p566
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [com.climate.claypoole :as cp]
            [code.util.io-utils :refer :all]
            [clojure.math.numeric-tower :as math])
  (:import [code.util InputReader]
           (java.util HashMap Collections HashSet TreeSet)
           (java.lang Math)
           ))


(defn opt-storage
  [bounds vals]
  ;; (println "args " bounds vals)
  (assert (== 0.0 (first bounds)))
  (assert (== 360.0 (last bounds) ))
  (assert (== 1 (- (count bounds) (count vals))))
  (loop [final-b [0.0]
         final-v []
         bds (rest (rest bounds))
         vls (rest vals)
         prv-v (first vals)
         prv-b (first (rest bounds))]
    (if (empty? vls)
      (do (assert (empty? bds))
          (assert (== 1 (- (count final-b) (count final-v))))
          (assert (== prv-b 360))
          [(conj final-b prv-b) (conj final-v prv-v)])
      (let [eq (== prv-v (first vls))]
        (recur
          (if eq final-b (conj final-b prv-b))
          (if eq final-v (conj final-v prv-v))
          (rest bds)
          (rest vls)
          (first vls)
          (first bds))))))

(defn app-comp
  [x y]
  (if (< (Math/abs (double (- x y))) 1e-06)
    0
    (compare x y )))

(defn run-next
  [bounds in-vals cur-point ang]
  (assert (< cur-point 360))
  (let [ins-point (Collections/binarySearch bounds cur-point app-comp)
        low-indx (if (>= ins-point 0) (+ ins-point 1) (* -1 (+ 1 ins-point)))
        [angs vals cur-point-f]
        (loop [cur-v cur-point ind low-indx
               angs [] vals [] ang-left ang]
          (let [next-ang (get bounds ind)
                next-val (get in-vals (- ind 1))]
            (if (nil? next-ang)
              (recur 0.0 1 angs vals ang-left)
              (if (or (zero? (app-comp next-ang (+ cur-v ang-left)))
                      (> next-ang (+ cur-v ang-left)))
                [(conj angs
                       ang-left) (conj vals next-val)
                 (+ cur-v ang-left)]
                (recur next-ang
                       (inc ind)
                       (conj angs (- next-ang cur-v))
                       (conj vals next-val)
                       (- ang-left (- next-ang cur-v)))))))
        ;; xx    (println ins-point low-indx angs vals)
        final-angs (reverse angs)
        final-vals (map #(bit-xor 1 %) (reverse vals))
        [mod-bounds mod-vals]
        (loop [first-bounds [cur-point]
               first-vals []
               cur-v cur-point
               angs final-angs
               vals final-vals]
          (if (empty? angs)
            (do (assert (empty? vals))
                [ first-bounds first-vals])
            (recur (conj first-bounds (+ cur-v (first angs)))
                   (conj first-vals (first vals))
                   (+ cur-v (first angs))
                   (rest angs)
                   (rest vals))))

        ;; xx (println "dd " bounds cur-point-f)
        high-ins-point (Collections/binarySearch bounds (double cur-point-f))
        high-indx (if (> high-ins-point 0) (+ high-ins-point 1)
                                           (* -1 (+ 1 high-ins-point)))
        ;xx (println "xx" cur-point ang bounds
        ;            ins-point low-indx angs vals
        ;            mod-bounds mod-vals cur-point-f
        ;            high-ins-point high-indx)
        [final-bounds final-vals]
        (if (> cur-point-f cur-point)
          (let [last-bounds (drop high-indx bounds)
                last-vals (drop   (- high-indx 1) in-vals)
                first-bounds (take (if (>= ins-point 0)
                                     (- low-indx 1) low-indx) bounds)
                first-vals (take (if (>= ins-point 0)
                                   (- low-indx 1) low-indx) in-vals)
                ]
            (opt-storage (concat first-bounds mod-bounds last-bounds)
                         (concat first-vals mod-vals last-vals)))
          (let [[lst frst] (split-with #(>= 360.0 %) mod-bounds)
                last-val (last lst)
                last-f (if (= 360.0 last-val) lst (concat lst [360.0]))
                last-v (take (- (count last-f) 1) mod-vals)
                frst (concat [0.0] (map #(mod % 360.0) frst))
                first-v (drop (- (count mod-vals)
                                 (- (count frst) 1)) mod-vals)
                last-val (last frst)
                mid-bds (drop high-indx
                              (take (if (>= ins-point 0)
                                      (- low-indx 1) low-indx) bounds))
                mid-vals (drop (- high-indx 1)
                               (take (if (>= ins-point 0)
                                       (- low-indx 1) low-indx) in-vals))]
            ;; (println "xxx" last-val last-f last-v frst first-v mid-bds mid-vals)
            (opt-storage (concat frst mid-bds last-f)
                         (concat first-v mid-vals last-v))))]
    [final-bounds final-vals cur-point-f]))

(defn check-sim
  [a b c n]
  (let [st (TreeSet. app-comp)]
    (loop [ang-seq (repeat (+ (/ n a) (/ n b)
                              (/ n (Math/sqrt c))))
           cur-itr 0
           p-sz 0
           p-val 0
           cur-v 0.0]
      (if (or (> p-val 100)
              (> cur-itr 1000))
        (do (println "Done vals are " p-val p-sz cur-itr))
      (let [new-v (+ cur-v (first ang-seq))
            new-v-final (if (>= (app-comp new-v n) 0)
                          (- new-v n) new-v)
            added (.add st new-v-final)]
         (println " vvvv " new-v new-v-final (first ang-seq))
        (recur (rest ang-seq)
               (inc cur-itr)
               (if added (inc p-sz) p-sz)
               (if added 0 (inc p-val))
               new-v-final))))))

(defn run-sim
  [a b c tries]
  (loop [ang-seq (repeat (+ (/ 360.0 a) (/ 360.0 b)
                           (/ 360.0 (Math/sqrt c))))
         nt 1
         bnds [0.0 360.0]
         vals [1]
         cur-v 0.0]
      (if (or (<= nt tries)
              (and (> nt 1) (== (count vals) 1)))
        (let [cur-v-final (if (>= cur-v 360.0) (- cur-v 360.0) cur-v)
              cur-ang (first ang-seq)
              [n-bds n-vals cur-v] (run-next bnds vals cur-v-final cur-ang)]

          (println "------------------ Run completed " nt)
          (println "Cur val is " cur-v-final cur-v cur-ang)
          ;; (println "New boundaries "  n-bds n-vals)
          (println "Boundary count " (count n-bds) (count n-vals))
          (recur (rest ang-seq)
                 (inc nt)
                 n-bds n-vals cur-v)))))
