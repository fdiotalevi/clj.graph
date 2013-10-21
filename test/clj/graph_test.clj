(ns clj.graph-test
  (:use [clj.graph])
  (:require [clojure.test :refer :all]))

(def test-graph (graph #{:a :b :c} #{#{:a :b}}))

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


(deftest recognise-adjacents
  (do
    (is (adjacent? test-graph :b :a))
    (is (not (adjacent? test-graph :a :c)))))

(deftest recognise-included-vertices
  (do
    (is (vertex? test-graph :a))
    (is (not (vertex? test-graph :d)))))
