(ns utils
  (:require [clojure.string :as str]))

(defn map-double [list] ;; Данная функция необходима для того, что бы правильно обрабатывать созданные отрезки, потому что появлятся точки вида: xx.999999999998
  (distinct (rseq (mapv #(Double/parseDouble (str/replace (format "%.2f" %) #"," ".")) (vec (into '() list))))))