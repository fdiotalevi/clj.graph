;; clj.graph implements the basic functions for graph
;; creation and manipulation in Clojure
(ns clj.graph)

(defrecord Graph [vertices edges])

(defn- valid-edges?
  ([edges]
     (if (empty? edges)
       true
       (let [valids (map #(and (set? %) (= 2 (count %))) edges)]
         (reduce #(and %1 %2) valids)))))

;; ## Define a graph

(defn graph
  "Create a graph, given the set of vertices and the set of edges.
  An edge is a set of 2 vertices
  f.i.

  `(graph #{:a :b :c} #{ #{:a :b} #{:a :c} })`

  Will throw an Exception if the set of vertices or edges is not correctly
  defined"
  [vertices edges]
  (if (and (set? vertices) (valid-edges? edges))
    (->Graph vertices edges)
    (throw (Exception. "Invalid graph"))))

;; ## Query the graph

(defn adjacent?
  "Given two vertices, check if they are adjacent"
  [the-graph vertex1 vertex2]
  (contains? (:edges the-graph) (hash-set vertex1 vertex2)))

(defn vertex?
  "Guven a vertex, check if it's part of the graph"
  [the-graph vertex]
  (contains? (:vertices the-graph) vertex))

;; ## Modify the graph

