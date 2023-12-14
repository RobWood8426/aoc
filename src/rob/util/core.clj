(ns rob.util.core
  (:require [clojure.string :as str]))

(defn parse-input [file-name]
  (->>
   file-name
   (str "resources/")
   slurp
   str/split-lines))

(defn reflect-array [field]
  (reduce
   (fn [acc row]
     (vec
      (map-indexed
       (fn [idx cell]
         (conj (vec (get acc idx)) cell))
       row)))
   []
   field))
