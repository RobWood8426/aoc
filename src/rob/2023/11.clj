(ns rob.2023.11
  (:require [rob.util.core :as uc]))

(defn all-dots? [xs]
  (every? #(= \. %) xs))

(defn expand-rows [field]
  (mapcat
   (fn [row]
     (cond
       (all-dots? row)
       [row row]

       :else
       [row]))
   field))

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

(comment
  (reflect-array
   (reflect-array
    ["abc"
     "def"
     "hjk"])))

(defn expand-input [field]
  (->>
   field
   expand-rows
   reflect-array
   expand-rows
   reflect-array))

(defn node-distance [[y' x'] [y x]]
  (+
   (abs (- y y'))
   (abs (- x x'))))

(defn between? [needle b1 b2]
  (or
   (<= b1 needle b2)
   (<= b2 needle b1)))

(defn node-distance-smart [factor [y' x'] [y x] rows cols]
  (let [row-crossings
        (count
         (filter
          identity
          (map
           #(between? % y y')
           rows)))
        col-crossings
        (count
         (filter
          identity
          (map
           #(between? % x x')
           cols)))]
    (+
     (* (dec factor) col-crossings)
     (* (dec factor) row-crossings)
     (abs (- y y'))
     (abs (- x x')))))

(comment
  (node-distance [0 4] [1 9]))

(defn find-nodes [field]
  (mapcat
   #(remove
     nil?
     %)
   (map-indexed
    (fn [row-idx row]
      (map-indexed
       (fn [col-idx cell]
         (when (= cell \#)
           [row-idx col-idx]))
       row))
    field)))

(defn all-distances [nodes]
  (let [node-count (count nodes)]
    (map-indexed
     (fn [idx node]
       (map
        #(node-distance node (nth nodes %))
        (range (inc idx) node-count)))
     nodes)))


(defn one []
  (let [input (uc/parse-input "2023-11.txt")]
    (->>
     input
     expand-input
     find-nodes
     all-distances
     (mapcat identity)
     (reduce +))))

(defn empty-rows [field]
  (remove
   nil?
   (map-indexed
    (fn [idx row]
      (cond
        (all-dots? row)
        idx))
    field)))

(comment
  (node-distance-smart 2 [1 9] [7 1] [5] [2]))

(defn all-distances-smart [nodes]
  (let [node-count (count nodes)]))


(defn two []
  (let [input (uc/parse-input "2023-11.txt")
        nodes (find-nodes input)
        expansion-rows (empty-rows input)
        expansion-cols (empty-rows (reflect-array input))
        node-count (count nodes)

        all-distances
        (map-indexed
         (fn [idx node]
           (map
            #(node-distance-smart 1000000 node (nth nodes %) expansion-rows expansion-cols)
            (range (inc idx) node-count)))
         nodes)]
    (reduce +
            (apply concat all-distances))))

(comment
  (one)
  (two))