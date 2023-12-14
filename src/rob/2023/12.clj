(ns rob.2023.12
  (:require [rob.util.core :as uc]
            [clojure.string :as str]))

(defn indexes-of-char [xs some-set]
  (keep-indexed
   (fn [idx v]
     (when (contains? some-set v) idx))
   xs))

(defn clean-data [input]
  (map
   (fn [line]
     (let [[springs instructions] (str/split line #" ")]
       [springs (map (fn [x] (Integer/parseInt x)) (str/split instructions #","))]))
   input))

(defn determine-options [line instruction])

(defn process-line [[open-spots instructions]]
  (let [next-ins (first instructions)
        instruction-space (reduce + instructions)]
    (cond
      (not (length-fit? open-spots instructions))
      false

      :else)))


(defn one []
  (let [data
        (clean-data
         (uc/parse-input "2023-12-test.txt"))]
    data))

(comment
  (one))