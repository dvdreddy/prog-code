(ns code.fbh-seventeen.r1.p3
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [swiss.arrows :refer [-<>]]
            [clojure.data.priority-map :refer [priority-map]]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           [java.lang Math]))

(defn progress
  [k fams {:keys [prev f1 f2] :as cur-state}]
  (cond
    (= prev (- k 1)) ()
    (and (= f1 (- k 1)) (= prev (- k 2)))
    [{:cur (second (get fams (- k 1)))
      :prev (- k 1) :f1 nil :f2 nil}]
    (and (nil? f1) (nil? f2))
    [{:cur (first (get fams (+ prev 1)))
      :prev prev :f1 (+ prev 1) :f2 nil}]
    (nil? f2)
    [{:cur (second (get fams f1))
      :prev f1 :f1 nil :f2 nil}
      {:cur (first (get fams (+ f1 1)))
       :prev prev :f1 f1 :f2 (+ f1 1)}]
    (and (not (nil? f1)) (not (nil? f2)))
    [{:cur (second (get fams f1))
      :prev f1 :f1 f2 :f2 nil}]
    :else (do (println "cur-state "
                       k cur-state)
              (println "families " fams)
              (assert false))))

(defn djikstra
  [edges start end v-count]
  (loop [mp (apply priority-map
                   (mapcat #(list %
                                  (if (== % start)
                                    0 Integer/MAX_VALUE))
                           (range v-count)))]
    (cond
      (== (val (first mp)) Integer/MAX_VALUE)
      -1
      (== (key (first mp)) end)
      (val (first mp))
      :else
      (let [[v dist] (first mp)]
        (recur (-<> mp (dissoc <> v)
                    (reduce (fn [nmp [nv ne]]
                              ;; (println nmp nv)
                              (if (or (== ne -1)
                                      (nil? (get nmp nv)))
                                nmp (update
                                      nmp nv
                                      #(min % (+ dist ne)))))
                       <> (get edges v))))))))

(defn main-log
  [edges fams]
  (let [k (count fams)
        last-fam (last fams)]
    (apply
      djikstra
      (loop [res {}
             states [{:cur 0
                      :prev -1
                      :f1 nil :f2 nil}]
             states-map {(first states) 0}
             next-state 0]
        ;; (println states-map)
        (if (empty? states)
          [res 0 (->> states-map
                      (filter #(let [{:keys [cur prev f1 f2]} (key %)]
                                 ;; (println "Cur " cur prev f1 f2)
                                 (and (== cur (second last-fam))
                                      (== prev (- k 1))
                                      (nil? f1) (nil? f2))))
                      first val) (inc next-state)]
          (let [cur-state (first states)
                cur-id (get states-map cur-state)
                [nres nstates nstates-map nnext-state]
                (reduce #(let [[r ns nsm nns] %1
                               state-id (get nsm %2)]
                           [(assoc-in r [cur-id (or state-id (inc nns))]
                                      (get-in edges [(:cur cur-state) (:cur %2)]))
                            (if state-id ns (conj ns %2))
                            (if state-id nsm (assoc nsm %2 (inc nns)))
                            (if state-id nns (inc nns))])
                        [res (rest states) states-map next-state]
                        (progress k fams cur-state))]
            (recur nres nstates nstates-map nnext-state)
            ))))))

(defn fw
  [n edges]
  (reduce
    (fn [edges [i j k]]
      (update-in edges
                 [i j]
                 #(let [vx (get-in edges [i k])
                        vy (get-in edges [k j])
                        tgt (if (or (== vx -1) (== vy -1))
                              -1 (+ vx vy))]
                    (if (or (== % -1) (== tgt -1))
                      (max tgt %)
                      (min tgt %)))))
    edges (for [k (range n)
                i (range n)
                j (range n)]
            [i j k])))


(defn get-input [reader]
  (let [n (.nextInt reader)
        m (.nextInt reader)
        k (.nextInt reader)
        edges (mapv #(assoc (into [] (repeat n -1)) % 0)
                    (range n))]
    [(fw n (reduce
             (fn [edges _]
               (let [x (dec (.nextInt reader))
                     y (dec (.nextInt reader))
                     wt (.nextInt reader)]
                 (-> edges
                     (update-in [x y]
                                #(if (= % -1)
                                   wt (min wt %)))
                     (update-in [y x]
                                #(if (= % -1)
                                   wt (min wt %))))))
             edges (range m)))
     (mapv (fn [_] [(dec (.nextInt reader))
                    (dec (.nextInt reader))])
           (range k))]))


(defn -main
  ([]
   (-main :stdout :stdin))
  ([output-file input-file]
   (delete-if-exists output-file)
   (let [reader (get-reader input-file)
         t (int (.nextInt reader))]
     (->> (range 1 (+ t 1))
          (map (fn [i]
                 (let [vals (get-input reader)]
                   (println "Processing Case" i)
                   (if (== i 45) (do (println "vals" vals)
                                     (str "Case #" i ": " (apply main-log vals)))
                                 ""))))
          (str/join "\n")
          (spit-internal output-file)))))
