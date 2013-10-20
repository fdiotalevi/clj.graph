(ns clj.graph-test
  (:use [clj.graph])
  (:require [clojure.test :refer :all]))

(deftest can-use-constructur
  (testing "can-construct"
    (is (not (nil? (graph #{} #{})))))
  (testing "can-associate-sets"
    (is (= #{:v} (:vertices (graph #{:v} #{}))))
    (is (= #{#{:a, :b}} (:edges (graph #{} #{#{:a, :b}}))))))

