(ns all-test
  (:require [clojure.test :refer [deftest testing is run-tests run-test]])
  (:require core)
  (:require linear)
  (:require utils)
  (:require lagrange))

(defn approximate? [expected actual tolerance]
  (<= (Math/abs (- expected actual)) tolerance))


(def two-point [[1 1] [3 3]])
(def point-to-search 2)
(def actual-y 2)

(deftest lagrange
  (testing "find-one-point"
    (is true)))

(deftest linear
  (testing "test-one-value"
    (is (= (linear/find-linear-interpolation 
            (first two-point) 
            (second two-point) 
            point-to-search) 
           actual-y) "found linear interpolation is correct"))
  (testing "many-values"
     (let [two-points-arrays [[[1 2] [4 5]] [[3.13 15.27] [8.56 65.2]] [[52.21 54.34] [125.16 652.23]]]
          points-to-search [3 7.21 102.54]
          actual-values [4 52.786 466.839]
          tolerance 0.01]
    
      (doseq [i (range (count two-points-arrays))]
        (let [expected (linear/find-linear-interpolation (first (nth two-points-arrays i))
                                                         (second (nth two-points-arrays i))
                                                         (nth points-to-search i))
              actual (nth actual-values i)]
          (is (approximate? expected actual tolerance) "check linear by array")))))
  (testing "wrapper-values"
    (let [points [[1. 2.] [3. 4.] [5. 6.]] 
          f 0.5 
          result (linear/linear-interpolation points f)
          expected-values [[3.0 4.0] [3.5 4.5] [4.0 5.0] [4.5 5.5] [5.0 6.0]]
          tolerance 0.01]
    
      (is (= (count result) 5) "new count is actual") 
      (doseq [i (range (count expected-values))]
        (let [actual (nth result i)
              expected (nth expected-values i)]
          (is (approximate? (second expected) (second actual) tolerance)))
    ))))


(deftest utils
  (testing "make-dobule-check"
    (let [actual '(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0)
          result (utils/map-double (conj (vec (range 0. 5. 0.5)) 5.))]
      (is (= actual result)))
    ))

(run-test utils)
(run-tests)