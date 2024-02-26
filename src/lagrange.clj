(ns lagrange
  (:require utils))

(defn find-lagrange-interpolation [points x]
  (let [n      (count points)
        p-func (fn [i]
                 (reduce * (for [j     (range n)
                                 :when (not (= j i))]
                             (/ 
                              (- x (first (nth points j))) 
                              (- (first (nth points i)) (first (nth points j)))))))]
    (reduce + (for [i (range n)]
                (* 
                 (second (nth points i)) 
                 (p-func i))))
    ))


(defn lagrange-interpolation [points f]
 
  (let [x-first (first (first points))
        x-last (first (last points)) 
        keys (conj (vec (range x-first x-last f)) x-last)
        keys-mapped (utils/map-double keys)
        values (mapv #(find-lagrange-interpolation points %) keys-mapped)
        values-mapped (utils/map-double values)]
    (mapv vector keys-mapped values-mapped)))

;; (lagrange-interpolation [[9.0 53.0] [10.0 34.0] [12.0 56.0]] 1)