(ns all-test
  (:require [clojure.test :refer [deftest testing is run-tests]])
  (:require core))

(deftest check1
  (testing "test1"
    (is (= 1 1))))