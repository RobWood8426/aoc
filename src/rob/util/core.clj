(ns rob.util.core 
  (:require [clojure.string :as str]))

(defn parse-input [file-name] 
  (->> 
   file-name
   (str "resources/")
   slurp
   str/split-lines))