(ns rob.2023.9 
  (:require [clojure.string :as str]
            [rob.util.core :as uc]))

(defn diffs [sequ]
  (let [len (dec (count sequ))
        vseq (vec sequ)]
    (reduce
     (fn [acc item]
       (conj acc (- (get vseq (inc item)) (get vseq item))))
     []
     (range len)))
  )

(comment 
  
  (diffs [1 2 3 4])
  (diffs [1 3 6 10 15 21])
  
  )


(defn next-item [line]
  (reduce 
   +
   (loop [current-line line
          prior-items []]
     (let [line' (diffs current-line)
           only-zero? (empty? (remove zero? line'))
           history (conj prior-items (last current-line))]
       (cond
         only-zero?
         history
         
         :else
         (recur 
          line'
          history)
         
         ))
     
     ))
  
  )

(comment
  (next-item [ 0 3 6 9 12 15])
  (next-item [1 3 6 10 15 21])
  )

(defn one []
  (let [input (uc/parse-input "2023-9.txt")
        clean-lines 
        (map 
         (fn [line] 
           (map
            (fn [x] (Integer/parseInt x))
            (str/split line #" "))) 
         input)]
    
    (->>
     clean-lines
     (map next-item)
     (reduce +))))

(defn two [] 
  (let [input (uc/parse-input "2023-9.txt")
        clean-lines
        (map
         (fn [line]
           (reverse
            (map
             (fn [x] (Integer/parseInt x))
             (str/split line #" "))))
         input)]
  
    (->>
     clean-lines
     (map next-item)
     (reduce +)))
  )

(comment
  (one)
  (two)
  )