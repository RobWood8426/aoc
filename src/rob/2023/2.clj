(ns rob.2023.2 
  (:require [clojure.string :as str]))

(defn input [file-name]
  (->>
   file-name
   slurp
   str/split-lines))

(defn valid-pair? [m-r m-g m-b pair]
  (let [[qty colour] (str/split (str/trim pair) #" ")
        int-qty (Integer/parseInt qty)]
    (case colour
      "red" (>= m-r int-qty)
      "green" (>= m-g int-qty)
      "blue" (>= m-b int-qty))))

(comment
  (valid-pair? 7 3 3 " 3 green")
  )

(def max-red 12)
(def max-green 13)
(def max-blue 14)



(defn one []
  (let [games (input "resources/2023-2-1.txt")]
    (reduce
     +
     (map
      first
      (filter
       second
       (map
        (fn [line]
          (let [[game raw-hands] (str/split line #":")
                hands (str/split raw-hands #";")]
            [(Integer/parseInt (last (str/split game #" ")))
             (empty?
              (remove
               identity
               (map
                (fn [hand]
                  (let [pairs (str/split hand #",")]
                    (empty?
                     (remove
                      identity
                      (map
                       (partial valid-pair? max-red max-green max-blue)
                       pairs)))))
                hands)))]))
        games))))
    ))

(defn two []
  (let [games (input "resources/2023-2-1.txt")]
    (reduce
     +
     (map
      (fn [line]
        (let [[game raw-hands] (str/split line #":")
              hands (str/split raw-hands #";")]
          (reduce
           *
           (vals
            (reduce
             (fn [acc pair]
               (let [[qty colour] (str/split (str/trim pair) #" ")
                     int-qty (Integer/parseInt qty)]
                 (case colour
                   "red" (assoc acc :m-r (max (:m-r acc) int-qty))
                   "green" (assoc acc :m-g (max (:m-g acc) int-qty))
                   "blue" (assoc acc :m-b (max (:m-b acc) int-qty)))))
             {:m-r 0
              :m-g 0
              :m-b 0}
             (vec
              (mapcat
               (fn [hand]
                 (let [pairs (str/split hand #",")]
                   pairs))
               hands)))))))
      games))))


(comment 
  (one)
  (two)
  )