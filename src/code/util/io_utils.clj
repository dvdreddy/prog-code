(ns code.util.io-utils
  (:refer-clojure)
  (:require [clojure.java.io :as java-io])
  (:import (java.io FileInputStream)
           (code.util InputReader)))


(defn get-reader
  [file-name]
  (if (= :stdin file-name)
    (InputReader. System/in)
    (InputReader. (FileInputStream. file-name))))


(defn delete-if-exists
  [file-name]
  (if (not (= :stdout file-name))
    (java-io/delete-file file-name true)))

(defn spit-internal
  [file-name output]
  (if (= :stdout file-name)
    (println output)
    (spit file-name output)))
