(ns linear
  (:require utils))

(defn find-linear-interpolation [[x1 y1] [x2 y2] x]
  (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1))))

(defn linear-interpolation [points f]
  (let [two-points (take 2 (rseq points))
        p-min (second two-points)
        p-max (first two-points)
        x1 (first p-min)
        y1 (second p-min)
        x2 (first p-max)
        y2 (second p-max)
        keys (conj (vec (range x1 x2 f)) x2)
        keys-mapped (utils/map-double keys)
        values (mapv #(find-linear-interpolation [x1 y1] [x2 y2] %) keys-mapped)
        values-mapped (utils/map-double values)]
    (mapv vector keys-mapped values-mapped)))

(linear-interpolation [[9.0 53.0] [10.0 34.0] [12.0 56.0]] 0.2)