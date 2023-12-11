(ns rob.2023.1
  (:require 
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn input [file-name]
  (->> 
   file-name
   slurp
   str/split-lines))


(defn one []
  (let [lines (input "resources/2023-1.txt")]
    (reduce
     +
     (map
      (fn [line]
        (let [filtered-line (re-seq #"\d" line)]
          (Integer/parseInt (str (first filtered-line) (last filtered-line)))))
      lines)))
  )

(def matches
  [["1" "1"]
   ["2" "2"]
   ["3" "3"]
   ["4" "4"]
   ["5" "5"]
   ["6" "6"]
   ["7" "7"]
   ["8" "8"]
   ["9" "9"]
   ["one" "1"]
   ["two" "2"]
   ["three" "3"]
   ["four" "4"]
   ["five" "5"]
   ["six" "6"]
   ["seven" "7"]
   ["eight" "8"]
   ["nine" "9"]])

(defn match-index 
  ([line match replacement]
   (match-index line match replacement [] -1))
  ([line match replacement acc offset]
   (let [line-index (str/index-of line match (inc offset))] 
     (cond
       line-index 
       (match-index
        line match replacement
        (conj acc [line-index replacement])
        line-index) 
       
       :else 
       acc))))

(defn line-matcher [line]
  (mapv
   last
   (sort-by
    first
    (into
     {}
     (mapcat
      #(apply (partial match-index line) %1)
      matches)))))

(defn two []
  (let [lines (input "resources/2023-1.txt") 
        
        line-nums
        (map 
         line-matcher
         lines)
        
        combined 
        (reduce
         +
         (map
          #(Integer/parseInt (str (first %) (last %)))
          line-nums))
        
        ] 
    combined
    )
  )




(comment 
  (one)
  (two)
  (take-last 20
             (two))
  
  
  
  
  (match-index "eightwothreeight" "eight" "8")
  
  (->
   "two1two"
   (str/replace #"two" "2")
   (str/replace #"three" "3")
   (str/replace #"four" "4")
   (str/replace #"five" "5")
   (str/replace #"six" "6")
   (str/replace #"seven" "7")
   (str/replace #"eight" "8")
   (str/replace #"nine" "9")
   
   )
  
  )