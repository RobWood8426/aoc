(ns rob.2023.12-2
  (:require [clojure.string :as str]
            [rob.util.core :as uc]))

(defn parse-line [line]
  (let [[row nrs] (str/split line #" ")]
    [(str (.replaceAll row "\\.\\.+" "."))
     (map parse-long (.split nrs ","))]))

(defn valid-suffixes [row number]
  (for [index (range (inc (- (count row) number))) ;; row size - needed spots + 1 (clipped range)
        :while (every? #{\. \?} (take index row)) ;; While all chars are either \. or \?  
        ;;in the range from your position for index items
        :when (every? #{\# \?} (take number (drop index row))) ;; go into body when the next n items are either broken or question-mark
        :when (contains? #{\. \?} (nth row (+ index number) \.))]
    (drop (+ index number 1) row)))

(defn acs [row numbers]
  (if-let [[n & nrs] (seq numbers)]
    (reduce
     +
     (for [s (valid-suffixes row n)] (acs s nrs)))
    (if (every? #{\. \?} row) 1 0)))

(def acs (memoize acs))


(comment
  "???.### 1,1,3"
  "?###???????? 3,2,1"
  (valid-suffixes "???.###" 1)
  (valid-suffixes "##?##" 2))


(comment
  (->> (uc/parse-input "2023-12-test.txt")
       (map parse-line)
       (map (partial apply acs))
       #_(reduce +)
       #_(println "First:")
       #_(time)))
;; First: 8270

(defn *5 [[line rule]]
  [(str/join "?" (repeat 5 line))
   (apply concat (repeat 5 rule))])

(comment
  (->> (slurp "2023-12-test.txt")
       (map parse-line)
       (map *5)
       (pmap (partial apply acs))
       (reduce +)
       (println "Second:")
       (time)))