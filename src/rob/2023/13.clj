(ns rob.2023.13
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [rob.util.core :as uc]))

(defn load-input []
  #_(take
     10)
  (mapv
   str/split-lines
   (->
    "resources/2023-13.txt"
    slurp
    (str/split #"\n\n"))))

(defn reflection-points [line]
  (reduce
   (fn [acc reflection-index]
     (let [lside (take reflection-index line)
           rside (nthrest line reflection-index)
           smallest-side (min (count lside) (count rside))
           lside (take smallest-side (reverse lside))
           rside (take smallest-side rside)
           mirror? (= lside rside)]
       (cond
         mirror?
         (conj acc reflection-index)

         :else
         acc)))
   []
   (range 1 (count line))))

(defn matches [lside rside]
  (map-indexed
   (fn [idx l]
     (if (= l (nth rside idx)) 0 1))
   lside))

(defn off-by-1-reflection-points [line]
  (reduce
   (fn [acc reflection-index]
     (let [lside (take reflection-index line)
           rside (nthrest line reflection-index)
           smallest-side (min (count lside) (count rside))
           lside (take smallest-side (reverse lside))
           rside (take smallest-side rside)
           mirror-off-by-1? (= lside rside)]
       (cond
         mirror-off-by-1?
         (conj acc reflection-index)

         :else
         acc)))
   []
   (range 1 (count line))))

(defn reflections-from-field [field]
  (apply
   set/intersection
   (map
    (fn [line]
      (set (reflection-points line)))
    field)))



(comment

  (reflections-from-field
   [".#..####..."
    "####.##.###"
    ".##.#...###"
    "##......###"
    "#######.#.."
    "#######.#.."
    "#.......###"
    ".##.#...###"
    "####.##.###"
    ".#..####..."
    "#...###...."
    ".##....#..."
    "###..##.###"
    ".##...#..##"
    "..##.###.##"])
  (reflection-points ".#..####...")

  (reflection-points "12211221")


  (nthrest 3 "hello"))

(defn one []
  (let [input (load-input)]
    (reduce
     +
     (mapcat
      (fn [field]
        (concat
         (reflections-from-field field)
         (map
          #(* 100 %)
          (reflections-from-field (uc/reflect-array field)))))

      input))))

(comment
  (one))