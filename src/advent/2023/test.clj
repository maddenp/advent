(ns advent.2023.test
  (:require [advent.2023 d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16]
            [clojure.test :refer [deftest is]]
            [eftest.runner :refer [find-tests run-tests]]))

(deftest d01 (is (= (advent.2023.d01/go) [53974 52840])))
(deftest d02 (is (= (advent.2023.d02/go) [2545 78111])))
(deftest d03 (is (= (advent.2023.d03/go) [520019 75519888])))
(deftest d04 (is (= (advent.2023.d04/go) [20667 5833065])))
(deftest d05 (is (= (advent.2023.d05/go) [403695602 219529182])))
(deftest d06 (is (= (advent.2023.d06/go) [32076 34278221])))
(deftest d07 (is (= (advent.2023.d07/go) [250058342 250506580])))
(deftest d08 (is (= (advent.2023.d08/go) [14893 10241191004509])))
(deftest d09 (is (= (advent.2023.d09/go) [1681758908 803])))
(deftest d10 (is (= (advent.2023.d10/go) [6640 411])))
(deftest d11 (is (= (advent.2023.d11/go) [9799681 513171773355])))
(deftest d12 (is (= (advent.2023.d12/go) [7653 60681419004564])))
(deftest d13 (is (= (advent.2023.d13/go) [41859 30842])))
(deftest d14 (is (= (advent.2023.d14/go) [108889 104671])))
(deftest d15 (is (= (advent.2023.d15/go) [516070 244981])))
(deftest d16 (is (= (advent.2023.d16/go) [7996 8239])))

(def test-ns "advent.2023.test")

(defn -main
  [& args]
  (let [test (first args)]
    (run-tests (find-tests (symbol (if test (format (str test-ns "/%s") test) test-ns))))))
