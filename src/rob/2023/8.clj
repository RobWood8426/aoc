(ns rob.2023.8 
  (:require [clojure.string :as str]
            [rob.util.core :as uc]))


(defn nodes->map [nodes]
  (->>
   nodes
   (map
    (fn [line]
      (let [[key-node val-node] (str/split line #" = ")
            clean-val (str/split (subs val-node 1 (dec (count val-node))) #", " )] 
        [key-node clean-val])
      ))
   (into {})))

(defn one []
  (let [[instructions' _ & nodes]
        (uc/parse-input "2023-8.txt")
        node-map (nodes->map nodes)
        instructions (cycle (seq instructions'))] 
    
    (loop [index 0 current-node "AAA"]
      (let [next-instruction (nth instructions index)
            next-node 
            (get-in 
             node-map 
             [current-node 
              (case next-instruction
                \L 0
                \R 1)])]
        (when (zero? (mod index 10)) (println index " - " current-node))
        (cond 
          (= current-node "ZZZ")
          index
          
          :else
          (recur (inc index) next-node)))
      )))

(defn step-node [node-map instruction current-node]
  #_(println node-map)
  (get-in
   node-map
   [current-node
    (case instruction
      \L 0
      \R 1)]))


;;Ignore for now
(defn distance-to-cycle [node node-map instructions ins-count]
  (loop [index 0 current-node node]
    (let [next-instruction (nth instructions index)
          next-node
          (get-in
           node-map
           [current-node
            (case next-instruction
              \L 0
              \R 1)])]
      (println index " : node?" (= current-node node) " zero-rem?" (zero? (mod index ins-count)) " ins-count:"ins-count)
      (cond
        (and
         (< 0 index)
         (= current-node node)
         (zero? (mod index ins-count))) 
        index
        
        (= index 20)
        nil
        
        :else
        (recur (inc index) next-node)))))

(defn distance-to-z [node node-map instructions ins-count]
  (loop [index 0 current-node node]
    (let [next-instruction (nth instructions index)
          next-node
          (get-in
           node-map
           [current-node
            (case next-instruction
              \L 0
              \R 1)])]
      (cond
        (= \Z (last current-node))
        index

        :else
        (recur (inc index) next-node)))))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))


(defn two []
  (let [[instructions' _ & nodes]
        (uc/parse-input "2023-8.txt")
        node-map (nodes->map nodes)
        ins-count (count instructions')
        instructions (cycle (seq instructions'))
        current-nodes (filter #(= (last %) \A) (keys node-map))]
    
    (apply lcmv (map #(distance-to-z % node-map instructions ins-count) current-nodes))))

(comment 
  (one)
  (two)
  
  (lcmv 2 3 1 2 6)
  )