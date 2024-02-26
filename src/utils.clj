(ns utils
  (:require [clojure.string :as str]))

(defn map-double [list]
  (distinct (rseq (mapv #(Double/parseDouble (str/replace (format "%.2f" %) #"," ".")) (vec (into '() list))))))