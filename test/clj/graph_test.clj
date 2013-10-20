(ns clj.graph-test
  (:use [clj.graph])
  (:require [clojure.test :refer :all]))

(deftest can-use-constructur
  (testing "can-construct"
    (is (not (nil? (graph #{} #{})))))
  (testing "can-associate-sets"
    (is (= #{:v} (:vertices (graph #{:v} #{}))))
    (is (= #{#{:a, :b}} (:edges (graph #{} #{#{:a :b}}))))))

(deftest can-validate-graph
  (testing "can-recognise-invalid-vertices"
    (try (do
           (graph {} #{#{:a :b}})
           (is false "Vertices validation not working"))
         (catch Exception e true))) 
  (testing "can-recognise-invalid-edges"
    (try (do
           (graph #{} #{#{:a}})
           (is false "Edges validation not working"))
         (catch Exception e true))))
