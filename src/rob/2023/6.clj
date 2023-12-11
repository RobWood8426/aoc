(ns rob.2023.6
  (:require 
   [rob.util.core :as uc]
   [clojure.string :as str]
   [clojure.walk :as walk]))

(defn determine-distance [race-time hold-time]
  (* 
   (- race-time hold-time)
   hold-time
   ))

(defn determine-wins [race-time race-record]
  (count
   (filter 
    #(> % race-record)
    (map
     #(determine-distance race-time %)
     (range 1 race-time))))
  )

(comment
  (determine-wins 7 9)
  (determine-distance 7 4)
  )

(defn one []
  (let [[rules records] 
        (map
         (fn [line]
           (vec 
            (map 
             (fn [item] 
               (Integer/parseInt 
                (str/trim item))) 
             (rest
              (remove 
               empty?
               (str/split line #"  "))))))
         (uc/parse-input "2023-6.txt"))]
    (reduce 
     *
     (map-indexed 
      (fn [idx rule]
        (determine-wins rule (get records idx))) 
      rules))
    ))

(defn two []
  (let [[rule record]
        (map
         (fn [line]
           (bigint
            (apply 
             str
             (mapcat
              (fn [item] 
                (str/trim item))
              (rest
               (remove
                empty?
                (str/split line #"  ")))))))
         (uc/parse-input "2023-6.txt"))]
    (determine-wins rule record)))

(comment 
  (one)
  (two)
  )