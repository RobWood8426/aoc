(ns rob.2023.5
  (:require [rob.util.core :as uc]
            [clojure.string :as str]
            [clojure.walk :as walk]))


(defn parse-int [stri]
  (bigint stri))
(defn range-fn [start length] 
  #(and 
   (>= % start)
   (> (+ start length) %))
  )

(comment
  ((range-fn 5 2) 8))

(defn parse-map [map-rules]
  (into
   {}
   (map
    (fn [rule]
      (let [[dest source length] (map parse-int (str/split rule #" "))]
        [(range-fn source length) (- dest source)]
        )
      )
    map-rules)))

(comment
  (parse-map ["50 98 2" "52 50 48"])
  )

(defn get-seeds [input]
  (str/split (last (str/split (first input) #": ")) #" "))

(defn get-seeds-ranges [input]
  (partition-all
   2
   (get-seeds input))
  )

(comment
  (get-seeds-ranges (uc/parse-input "2023-5-test.txt"))
  )

(defn get-transfers [input]
  (reduce
   (fn [acc line]
     (let [latest-map (last acc)]
       (cond
         (str/includes? line "map:")
         (conj acc [])
  
         (= line "")
         acc
  
         :else
         (conj (vec (butlast acc)) (conj latest-map line)))))
   []
   (nthrest input 2)))

(defn process-seeds [transfer-maps seeds]
  (apply
   min
   (map
    (fn [seed]
      (reduce
       (fn [current-val transfer-map]
         (let [valid-fn
               (ffirst
                (filter
                 (fn [[check-fn]]
                   (check-fn current-val))
                 transfer-map))]
           (cond
             valid-fn
             (+ current-val (get transfer-map valid-fn))
  
             :else
             current-val)))
       (parse-int seed)
       transfer-maps))
    seeds)))

(defn one []
  (let [input (uc/parse-input "2023-5.txt")
        seeds (get-seeds input) 
        transfers (get-transfers input) 
        transfer-maps (map parse-map transfers)]
    (process-seeds transfer-maps seeds)))

(defn two []
  (let [input (uc/parse-input "2023-5.txt")
        seeds (get-seeds-ranges input)
        transfers (get-transfers input)
        transfer-maps (map parse-map transfers)]
    #_(process-seeds transfer-maps seeds)
    
    (pmap
     (fn [[start-seed length]]
       (let
        [seed (parse-int start-seed)
         len (parse-int length)]
         (loop [c-seed seed
                c-len len
                current-min (bigint 999999999)]
           
           (when (= (mod (- c-seed seed) 10000) 0) 
             (println "seed:" (apply str (take 5 (str seed))) " so far: " (float (* 100 (/ (- c-seed seed) len)))))
           
           (cond
             (> 0 c-len)
             current-min
             
             :else
             (recur
              (+ c-seed 100)
              (- c-len 100) 
              (min 
               current-min
               (process-seeds transfer-maps (range c-seed (+ c-seed (min 100 c-len))))))))))
     seeds))
  )

(comment 
  (one)
  (two)
  )








  ;;(:require [clojure.string :as str]
  ;;          [clojure.walk :as walk])
  ;;(:import [clojure.lang PersistentQueue])

(defn locmap<-digits
  ([s] (locmap<-digits s identity))
  ([s f]
   (let [lmap
         (->> s
              (str/split-lines)
              (map-indexed vector)
              (mapcat (fn [[r l]]
                        (map-indexed (fn [c v]
                                       [[r c] (f (Character/digit v 10))]) l)))
              (into {}))
         size (reduce (fn [[rm cm] [r c]] [(max rm (inc r)) (max cm (inc c))])
                      [0 0]
                      (keys lmap))]
     {:locmap lmap :size size})))

(defn locmap<-
  ([s] (locmap<- identity s))
  ([f s]
   (let [lmap
         (->> s
              (str/split-lines)
              (map-indexed vector)
              (mapcat (fn [[r l]] (map-indexed (fn [c v] [[r c] (f v)]) l)))
              (into {}))
         size (reduce (fn [[rm cm] [r c]] [(max rm (inc r)) (max cm (inc c))])
                      [0 0]
                      (keys lmap))]
     {:locmap lmap :size size})))

(defn parse-longs [s]
  (map parse-long (re-seq #"-?\d+" s)))

(defn split-grouped-lines [input]
  (->> input
       str/split-lines
       (partition-by empty?)
       (take-nth 2)))

(defn transpose
  ([ll] (transpose nil ll))
  ([pad ll]
   (->> ll
        (map #(concat % (repeat ::end)))
        (apply map vector)
        (take-while #(not-every? #{::end} %))
        (walk/postwalk-replace {::end pad}))))

(comment
  (transpose \space [[1 2 3] [4 5] [7 8 9]]))

(defn range-inc
  ([end] (range (inc end)))
  ([start end] (range start (inc end)))
  ([start end step] (range start (inc end) step)))

(defn range-x
  "Return range from `start` to `end`, inclusive. `end` need not be greater than
   `start`. Adapted from zelark (so it works even if start == end)."
  ([end] (range-inc 0 end))
  ([start end]
   (cond
     (< start end) (range start (inc end))
     (< end start) (range start (dec end) -1)
     :else (repeat start))))

(defn points-along [[x1 y1] [x2 y2]]
  (map vector (range-x x1 x2) (range-x y1 y2)))

(defn z-combinator
  "Takes function `g` of n+1 parameters, the first being a memoized function to
   be called recursively with the other n.

   ```
   (defn my-memoized-recursive-fn [args to close over]
     (z-combinator
      (fn [f arg1 ... argn]
        ...
        (f val1 ... valn)))
   ```
  "
  [g]
  (let [fix (fn [f] (fn z [& args] (apply f z args)))]
    (fix (memoize g))))

;; another handy gem from @zelark


#_
(defn parse-map [lines]
  (map parse-longs (drop 1 lines)))

(defn convert-with-range [[vstart vl :as aval] [dest src len]]
  (let [vend (+ vstart vl)
        range-end (+ src len)]
    (cond
      (<= (+ vstart vl) src) [aval]
      (< vstart src) (if (<= vend range-end)
                   [[vstart (- src vstart)] (reduced [dest (- vl (- src vstart))])]
                   [[vstart (- src vstart)] (reduced [dest len]) [(+ src len) (- (+ vstart vl) (+ src len))]])
      (< vstart (+ src len)) (if (<= (+ vstart vl) (+ src len))
                           [(reduced [(+ dest (- vstart src)) vl])]
                           [(reduced [(+ dest (- vstart src)) (- (+ src len) vstart)]) [(+ src len) (- (+ vstart vl) (+ src len))]])
      :else [aval])))

(defn convert-with-map [aval ranges]
  (->>
   (reduce
    (fn [val's arange]
      (mapcat
       #(if (reduced? %)
          [%]
          (convert-with-range % arange))
       val's))
    [aval]
    ranges)
   (map unreduced)))

(defn convert [aval starting-maps]
  (reduce 
   (fn [val's current-map] 
     (mapcat 
      #(convert-with-map % current-map) 
      val's))
   [aval]
   starting-maps))

(comment
  (def input (slurp "resources/2023-5-test.txt")) 
  (def input (slurp "resources/2023-5.txt"))

  (convert-with-range [53 1] [49 53 8])
  (convert-with-map [53 1] [[49 53 8] [0 11 42]])

  ;; year 2023 day 05 puzzle 1
  (let [[seeds & maps] (split-grouped-lines input)
        seeds (parse-longs (first seeds))
        maps (map parse-map maps)]
    (->> (mapcat #(convert % maps) (for [v seeds] [v 1]))
         (map first)
         (apply min)))   ;; => 51580674
  
  ;; year 2023 day 05 puzzle 2
  (let [[seeds & maps] (split-grouped-lines input) 
        seeds (parse-longs (first seeds)) 
        maps (map parse-map maps) 
        seeds (partition 2 seeds)]
    (->> (mapcat #(convert % maps) seeds)
         #_#_(map first)
         (apply min)))   ;; => 99751240
  )