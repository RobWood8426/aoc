(ns rob.2023.10
  (:require 
   [clojure.string :as str]
   [rob.util.core :as uc]))


(defn find-start [field]
  (reduce
   (fn [idx row] 
     (let [found-index (.indexOf row \S)]
       (cond
         (>= found-index 0)
         (reduced [idx found-index])
         
         :else (inc idx))))
   0
   field))

(def char->steps
  {\| #{[-1 0] [1 0]}
   \- #{[0 -1] [0 1]}
   \L #{[-1 0] [0 1]}
   \J #{[-1 0] [0 -1]}
   \7 #{[1 0] [0 -1]}
   \F #{[1 0] [0 1]}})

(defn ->step [[y' x'] [y x]]
  [(+ y y')
   (+ x x')])

(def cardinal-points
  [[0 1]
   [0 -1]
   [1 0]
   [-1 0]])

(defn one []
  (let [input
        (vec 
         (map 
          vec
          (uc/parse-input "2023-10-test.txt")))
        
        starting-point (find-start input)]
    (map
     #(get-in input (->step starting-point %))
     cardinal-points)
    
    )  
  )

(comment 
  (one)
  )