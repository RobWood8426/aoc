(ns rob.2023.4
  (:require [aoc.util.core :as uc]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math :as math]))

(defn one []
  (let [input (map #(last (str/split % #":")) (uc/parse-input "2023-4.txt"))]
    
    (reduce 
     +
     (map 
      (fn [line] 
        (let [[answers wins] (str/split line #"\|")
              intersections 
              (remove
               empty?
               (set/intersection
                (set (str/split answers #" "))
                (set (str/split wins #" "))))
              
              int-count (count intersections)] 
          (cond
            (empty? intersections)
            0
            
            :else
            (math/pow 2 (dec int-count))) 
          )
        )
      input)))
  )

(defn intersection-count [card-line]
  (let [clean-line (last (str/split card-line #":"))
        [answers wins] (str/split clean-line #"\|")
        intersections
        (remove
         empty?
         (set/intersection
          (set (str/split answers #" "))
          (set (str/split wins #" "))))

        int-count (count intersections)]
    int-count)
  )

(defn update-ref [current-ref card-idx more-mult]
  (let [current-mult (get-in current-ref [card-idx :mult])]
    (assoc-in 
     current-ref
     [card-idx :mult] (+ current-mult more-mult))))

(defn ref-update-range [current-ref new-range current-mult]
  
  
  #_(println current-ref ":" new-range ":" current-mult)
  (reduce
   (fn [old-ref card-idx]
     (update-ref old-ref card-idx current-mult))
   current-ref
   new-range))

(defn two []
  (let [input (uc/parse-input "2023-4.txt")
        
        solution-ref
        (apply 
         merge
         (map-indexed
          (fn [idx line]
            (let [card-idx (inc idx)]
              {card-idx
               {:next-range 
                (range
                 (inc card-idx)
                 (+ (inc card-idx) 
                    (intersection-count line))) 
                :mult 1}}))
          input))
        
        initial-cards (vec (keys solution-ref))
        
        card-total (count initial-cards)
        
        processed-lines
        (loop [processed-cards []
               current-ref solution-ref]
          
          
          (let [next-card (inc (count processed-cards))
                card-solution-map (get current-ref next-card)]
            
            
            #_(println "range: " (:next-range card-solution-map) "Multi: " (:mult card-solution-map))
            (cond
              (> next-card card-total)
              processed-cards
              
              :else
              (recur
               (doall (conj processed-cards [next-card (:mult card-solution-map)]))
               (ref-update-range current-ref (:next-range card-solution-map) (:mult card-solution-map))))))
        ]
    
    #_processed-lines
    (reduce 
       +
       (map 
        (fn [[_x y]]
          y)
        processed-lines))
    
    
    )
  )

(comment 
  (one)
  (two)
  )