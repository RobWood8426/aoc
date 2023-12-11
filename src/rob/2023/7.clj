(ns rob.2023.7 
  (:require [clojure.string :as str]
            [rob.util.core :as uc]))


(def types
  {[1 1 1 1 1] 0
   [1 1 1 2] 1
   [1 2 2] 2
   [1 1 3] 3
   [2 3] 4
   [1 4] 5
   [5] 6
   }
  )
(defn hand-type [hand]
  (->
   hand
   frequencies
   vals
   sort
   vec
   types))

(comment
  (hand-type "JJJQQ")
  (hand-type "JJJK2")
  )

(def values-two (reverse [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J]))
(def values-one (reverse [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2]))

(defn hand->values [hand values] 
  (map 
   #(.indexOf values %)
   hand)
  )

(comment 
  (hand->values "T55J5" values-two)
  )

(defn one []
  (let [ordering-list
        (into
         {}
         (map
          (fn [line]
            (let [[hand bid] (str/split line #" ")]
              [[(hand-type hand) (hand->values hand values-one)] (Integer/parseInt bid)]))
          (uc/parse-input "2023-7.txt")))
        
        sorted-list
        (sort-by
         (fn [row]
           (let [[grouping hand] (key row)]
             (vec (concat [grouping] hand))))
         ordering-list)] 
    (reduce 
     +
     (map-indexed
      (fn [idx [_ bid]]
        (* bid (inc idx))
        )
      sorted-list))
    ))

(defn clean-hand [hand]
  (let [j-char
        (or
         (first
          (last
           (sort-by
            last
            (frequencies 
             (remove
              #(= \J %)
              hand)))))
         \A)]
    (str/replace hand #"J" (str j-char))))
(comment
  (clean-hand "T55J5")
  )

(defn two []
  (let [ordering-list
        (into
         {}
         (map
          (fn [line]
            (let [[hand bid] (str/split line #" ")]
              [[(hand-type 
                 (clean-hand hand)) (hand->values hand values-two)] (Integer/parseInt bid)]))
          (uc/parse-input "2023-7.txt")))

        sorted-list
        (sort-by
         (fn [row]
           (let [[grouping hand] (key row)]
             (vec (concat [grouping] hand))))
         ordering-list)]
    (reduce
     +
     (map-indexed
      (fn [idx [_ bid]]
        (* bid (inc idx))) 
      sorted-list))))


(comment 
  
  (one)
  (two)
  
  )