(ns code.gcj-fifteen.one-c.prob-b
  (:refer-clojure)
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [code.util.io-utils :refer :all])
  (:import [code.util InputReader]
           (com.google.common.collect TreeMultiset)))



(defn get-kmp-tab
  [targ]
  (let [sz (count targ)]
    (loop [pos 2 cnd 0 tab [-1 0]]
      (if (<= pos sz)
        (if (= (.charAt targ (- pos 1))
               (.charAt targ cnd))
          (recur (+ pos 1) (+ cnd 1) (conj tab (+ cnd 1)))
          (if (> cnd 0)
            (recur pos (nth tab cnd) tab)
            (recur (+ pos 1) 0 (conj tab 0))))
        tab))))

(defn get-probs
  [s]
  (let [char-map (frequencies s)
        tot (count s)]
    (apply merge
           (map #(array-map
                  (key %) (double (/ (val %) tot)))
                char-map))))


(defn get-max-cnt
  [kmp-tab sz l]
  (+ 1 (int (/ (- l sz)
               (- sz (nth kmp-tab sz))))))


(defn get-init-map
  [sz]
  (assoc (->> (range sz)
              (map #(hash-map % (double 0.0)))
              (apply merge))
    0 (double 1.0)))


(defn get-nxt-st
  [kmp-tab cur-char s pr-st]
  (loop [cur-st pr-st]
    (if (= cur-char (.charAt s cur-st))
      (+ cur-st 1)
      (if (= cur-st 0)
        0
        (recur (nth kmp-tab cur-st))))))


(defn main-log
  [k l s alph targ]
  (let [kmp-tab (get-kmp-tab targ)
        probs (get-probs alph)
        sz (count targ)
        max-cnt (get-max-cnt kmp-tab sz l)
        exp (atom (double 0.0))]
    (loop [cur 0

           prob-map (get-init-map sz)]
      (if (< cur l)
        (let [new-mp (assoc (->> (range sz)
                                 (map #(hash-map % (double 0.0)))
                                 (apply merge))]
          (doall (map (fn [entr]
                        (let [key-char (key entr)
                              prob (val entr)]
                          (doall (map (fn [st-t]
                                        (let [pr-st (key st-t)
                                              prob-st (val st-t)
                                              nxt-st (get-nxt-st
                                                       kmp-tab key-char
                                                       targ pr-st)]
                                          (if (= nxt-st sz)
                                            (do (swap! exp (partial
                                                             + (* prob-st prob)))



                                            )


                                          ))
                                      prob-map))))
                      probs)

          )


        )



      )))


(defn -main
  [input-file output-file]
  (delete-if-exists output-file)
  (let [reader (get-reader input-file)
        t (int (.nextInt reader))]
    (->> (range 1 (+ t 1))
         (map (fn [i]
                (let [k (.nextInt reader)
                      l (.nextInt reader)
                      s (.nextInt reader)
                      alph (.next reader)
                      targ (.next reader)]
                  (println "Processing Case" i)
                  (str "Case #" i ": "
                       (main-log k l s alph targ)))))
         (str/join "\n")
         (spit output-file))))
