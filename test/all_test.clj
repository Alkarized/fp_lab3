(ns all-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require core)
  (:require linear)
  (:require utils)
  (:require lagrange))

(defn approximate? [expected actual tolerance]
  (<= (Math/abs (- expected actual)) tolerance))

(def dot-apr 0.01)
(def two-point [[1 1] [3 3]])
(def point-to-search 2)
(def actual-y 2)

(deftest lagrange
  (testing "find-one-point"
    (let [array-points [[1 1] [2 5] [10 25] [16 18]]
          point 9
          actual 24.02
          result (lagrange/find-lagrange-interpolation array-points point)]
      (is (approximate? actual result dot-apr))))
  (testing "many-dots"
    (let [array-points-2 [[1. 1.] [2. 5.] [10. 25.] [16. 18.]]
          f 1
          result-2 (lagrange/lagrange-interpolation array-points-2 f)
          actual-2 [[1.0 1.0] [2.0 5.0] [3.0 8.76] [4.0 12.23] [5.0 15.38] [6.0 18.17] [7.0 20.57] [8.0 22.53] [9.0 24.02] [10.0 25.0] [11.0 25.43] [12.0 25.27] [13.0 24.49] [14.0 23.04] [15.0 20.89] [16.0 18.0]]]

      (is (= (count result-2) (+ (- 16 1) 1)))

      (doseq [i (range (count result-2))]
        (is (approximate? (second (nth result-2 i)) (second (nth actual-2 i)) dot-apr))))))

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
          (is (approximate? (second expected) (second actual) tolerance)))))))

(deftest utils
  (testing "make-dobule-check"
    (let [actual '(0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0)
          result (utils/map-double (conj (vec (range 0. 5. 0.5)) 5.))]
      (is (= actual result)))))

(run-tests)