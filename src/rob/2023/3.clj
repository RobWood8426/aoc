(ns rob.2023.3
  (:require
   [aoc.util.core :as uc]))

(defn get-char [field row column]
  (str (get-in field [row column])))

(defn is-number? [current-num]
  (and
   current-num
   (re-find #"\d" current-num)))

(defn number-start? [field row column]
  (let [current-char (get-char field row column)
        prior-char (get-char field row (dec column))]
    (and
     (is-number? current-char)
     (not (is-number? prior-char)))))

(defn number-end? 
  ([row column-idx] 
   (number-end? [row] 0 column-idx)) 
  ([field row-idx column-idx]
   (let [current-char (get-char field row-idx column-idx)
         next-char (get-char field row-idx (inc column-idx))]
     (and
      (is-number? current-char)
      (not (is-number? next-char))))))

(comment
  (number-end? ["a" "b" "1" "2" "3" "a" "v" "1"] 4)
  )

(defn rest-of-number [row start-idx] 
  (let [start-trimmed (vec (drop start-idx row))]
    (apply
     str
     (second
      (reduce
       (fn [[idx found-number] _some-char] 
         (cond
           (and
            (not found-number)
            (number-end? start-trimmed idx))
           [idx (take (inc idx) start-trimmed)]

           :else
           [(inc idx) found-number]))
       [0 nil]
       start-trimmed)))))

(comment 
  (rest-of-number ["a" "b" "1" "2" "3" "a" "v" "1"] 2)
  )

(defn find-numbers [field] 
  (remove 
   nil?
   (apply
    concat
    (map-indexed
     (fn [row-idx row] 
       (map-indexed
        (fn [col-idx cell]
          (when (number-start? field row-idx col-idx)
            [row-idx col-idx (rest-of-number row col-idx)]))
        row))
     field))))

(defn find-stars [field]
  (remove
   nil?
   (apply
    concat
    (map-indexed
     (fn [row-idx row]
       (map-indexed
        (fn [col-idx cell]
          (when (= \* cell)
            [row-idx col-idx]))
        row))
     field)))
  )

(defn has-special? [[start-row-idx start-col-idx current-num] field]
  (not
   (empty?
    (remove
     #(or
       (empty? %)
       (= "." %)
       (re-find #"\d" %))
     (mapcat 
      (fn [offset]
        (let [current-col (+ offset (dec start-col-idx))]
          [(get-char field (dec start-row-idx) current-col)
           (get-char field start-row-idx current-col)
           (get-char field (inc start-row-idx) current-col)])
        )
      (range (+ 2 (count current-num))))))))

(comment
  (has-special? [0 5 "114"] (uc/parse-input "2023-3-test.txt"))
  )

(defn one []
  (let [input (uc/parse-input "2023-3.txt")
        found-numbers (find-numbers input)] 
    (reduce
     +
     (map
      (fn [tuple]
        (Integer/parseInt (last tuple)))
      (filter
       #(has-special? % input)
       found-numbers)))))

(defn number->sets [[row-idx col-idx current-num]]
  {(set
    (map 
     (fn [offset]
       [row-idx (+ col-idx offset)])
     (range (count current-num))))
   current-num})

(comment
  (number->sets [2 6 "633"])
  )

(defn star-surrounds [[row-id col-id]]
  (for [current-row (range (dec row-id) (+ row-id 2))
        current-col (range (dec col-id) (+ col-id 2))]
    [current-row current-col]))

(comment 
  (star-surrounds [2 6])
  )

(defn two []
  (let [input (uc/parse-input "2023-3.txt")
        found-stars (find-stars input)
        found-numbers (find-numbers input)
        number-reference
        (apply
         merge
         (map
          number->sets
          found-numbers))

        number-sets (keys number-reference)

        matching-sets
        (map
         (fn [star-coord]
           (vec
            (set
             (remove
              empty?
              (map
               (fn [co-ord]
                 (first
                  (remove
                   nil?
                   (map
                    (fn [current-set]
                      (when (contains? current-set co-ord)
                        current-set))
                    number-sets))))
               (star-surrounds star-coord))))))
         found-stars)
        
        valid-nums
        (filter
         (fn [matching-nums]
           (= (count matching-nums) 2))
         (map
          (fn [matches]
            (vec
             (set
              (map
               (fn [match]
                 (get number-reference match))
               matches))))
          
          matching-sets))]
    (reduce
     +
     (map
      (fn [nums]
        (reduce
         * 
         (map (fn [current-num] (Integer/parseInt current-num)) 
              nums)))
      valid-nums)) 

    ))

(comment
  (one)
  (two)
  ) 