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

(deftest can-tell-vertices-number
  (is (= 3 (v? test-graph))))

(deftest can-tell-edges-number
  (is (= 1 (e? test-graph))))

(deftest recognise-adjacents
  (do
    (is (adjacent? test-graph :b :a))
    (is (not (adjacent? test-graph :a :c)))))

(deftest recognise-included-vertices
  (do
    (is (vertex? test-graph :a))
    (is (not (vertex? test-graph :d)))))

(deftest can-add-vertices
  (testing "can add one vertex"
    (let [a-graph (add-vertices test-graph :d)]
      (do
        (is (not (vertex? test-graph :d)))
        (is (vertex? a-graph :d)))))
  (testing "can add multiple vertices"
    (let [a-graph (add-vertices test-graph :x :y :z)]
      (is (= 6 (count (:vertices a-graph)))))))

(deftest can-add-edges
  (testing "can add one edge"
    (is (not (adjacent? test-graph :b :c)))
    (let [new-graph (add-edges test-graph #{:b :c})]
      (is (adjacent? new-graph :b :c))))
  (testing "can add multiple edges"
    (let [new-graph (add-edges test-graph #{:b :c} #{:c :a})]
      (is (adjacent? new-graph :b :c))
      (is (adjacent? new-graph :a :c))))
  (testing "ignores non valid edges"
    (let [new-graph (add-edges test-graph #{:a :b :c} :a)]
      (is (= 1 (e? new-graph))))))

(deftest graph-equality-works
  (let [g1 (graph #{:a :b :c} #{#{:b :a} #{:a :c}})
        g2 (graph #{:c :b :a} #{#{:a :b} #{:c :a}})]
    (is (= g1 g2))))

(deftest can-build-a-cyclic-graph
  (let [g1 (cyclic-graph 3)]
    (do
      (is (= #{1 2 3} (:vertices g1)))
      (is (= #{#{1 2} #{2 3} #{3 1}} (:edges g1))))))

(deftest can-build-a-null-graph
  (let [ng (null-graph 10)]
    (is (= 10 (v? ng)))
    (is (= 0 (e? ng)))))

(deftest can-build-a-complete-graph
  (let [k3 (complete-graph 3)
        k4 (complete-graph 4)]
    (is (= k3 (cyclic-graph 3)))
    (is (= k4 (graph #{1 2 3 4} #{#{1 2} #{1 3} #{1 4} #{2 3}
                                  #{2 4} #{3 4}})))))

(deftest can-remove-edges
  (let [g (add-edges test-graph #{:b :c})]
    (testing "can remove one edge"
      (is (= 1 (e? (remove-edges g #{:a :b})))))
    (testing "can remove multiple edges"
      (is (= (graph #{:a :b :c} #{}) (remove-edges g #{:a :b} #{:b :c}))))))

(deftest can-sanitize-a-graph
  (testing "sanitize will remove useless edges"
      (let [g  (add-edges test-graph #{1 2})]
        (is (= (sanitize g) test-graph)))))

(deftest can-remove-vertices
  (testing "can remove one vertex and related edge"
    (let [c2 (cyclic-graph 2)
          r2 (remove-vertices c2 1)]
      (is (= 2 (v? c2)))
      (is (= 1 (e? c2)))
      (is (= 1 (v? r2)))
      (is (= 0 (e? r2)))))
  (testing "can remove multiple vertices and related edges"
    (is (= (remove-vertices (cyclic-graph 3) 3) (cyclic-graph 2)))))
