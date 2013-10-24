;; ## clj.graph
;;
;; clj.graph implements the basic functions for graph
;; creation and manipulation in Clojure

(ns clj.graph)

(defrecord Graph [vertices edges])

(defn- valid-edge?
  "Check if the provided edge is valid"
  [edge]
  (and (set? edge) (= 2 (count edge))))

(defn- valid-edges?  
  "Check if the provided sequence of edges are all valid"
  [edges]
  (if (empty? edges)
    true
    (let [valids (map valid-edge? edges)]
      (reduce #(and %1 %2) valids))))

(defn i-range
  "Inclusive-Inclusive range between 1 and `number`"
  [number]
  (range 1 (+ 1 number)))

;; ## Define a graph

(defn graph
  "Create a graph, given the set of vertices and the set of edges.
  An edge is a set of 2 vertices,
  f.i.

  `(graph #{:a :b :c} #{ #{:a :b} #{:a :c} })`

  Will throw an Exception if the set of vertices or edges is not correctly
  defined"
  [vertices edges]
  (if (and (set? vertices) (valid-edges? edges))
    (->Graph vertices edges)
    (throw (Exception. "Invalid graph"))))

;; ## Common graphs
;;
;; This section contains the definition of several popular graphs

(defn null-graph
  "Graph with `vertex-number` vertices and no edges"
  [vertex-number]
  (graph (set (i-range vertex-number)) #{}))

(defn- build-cycle
  "Utilty function to build the edges of a cyclic graph"
  [sequence]
  (if (< (count sequence) 2)
    #{}
    (let [max-sequence (last sequence)]
      (set (map #(hash-set % (if (= % max-sequence) 1 (+ 1 %))) sequence)))))

(defn cyclic-graph
  "Create a cyclic graph with the specified `number` of vertices.
  The vertices will be labeled from 1 to `number`, f.i.

  `(cyclic-graph 3)`

  will return a graph with vertices `1 2 3` and edges `(1 2) (2 3) (3 1)`"
  [number]
  (let [sequence (i-range number)
        vertices (set sequence)]
    (graph vertices (build-cycle sequence))))

(defn complete-graph
  "Create a complete graph with `number` vertices. In a complete graph
   all the vertices are adjacent.

  `(complete-graph 3)`

  will return a graph with vertices `1 2 3` and edges `(1 2) (1 3) (2 3)`"
  [number]
  (let [vertices (set (i-range number))
        unfiltered-edges (for [x vertices y vertices] (hash-set x y))
        edges (set (filter #(= 2 (count %)) unfiltered-edges))]
    (graph vertices edges)))

;; ## Query the graph

(defn v?
  "Return the number of vertices of a graph"
  [the-graph]
  (count (:vertices the-graph)))

(defn e?
  "Return the number of edges of a graph"
  [the-graph]
  (count (:edges the-graph)))

(defn adjacent?
  "Given two vertices, check if they are adjacent,
  f.i.

  `(adjacent? my-graph :a :b)`"
  [the-graph vertex1 vertex2]
  (contains? (:edges the-graph) (hash-set vertex1 vertex2)))

(defn vertex?
  "Given a vertex, check if it's part of the graph,
  f.i.

  `(vertex? my-graph :a)`"
  [the-graph vertex]
  (contains? (:vertices the-graph) vertex))

;; ## Modify the graph
;;
;; As often happens in Clojure, graphs are immutable structures;
;; therefore "modifying" a graph actually returns a copy of the 
;; original graph with the required modifications

(defn sanitize
  "This function checks whether the graph is correctly defined, f.i. if all the edges
  refer to existing vertices. It will return 
  - the same graph if it is correctly defined
  - a sanitized graph if not
  - an exception if the graph cannot be sanitized at all"
  [the-graph]
  (let [vertices (:vertices the-graph)
        edges (:edges the-graph)
        filtered-edges (set (filter #(and (contains? vertices (first %)) (contains? vertices (second %))) edges))]
    (graph vertices filtered-edges)))

(defn add-vertices
  "Add one or more vertices to the specified graph,
   f.i.

  `(add-vertices my-graph :x :y :z)`

  Will return a copy of my-graph with the additional vertices"
  [the-graph & new-vertices]
  (graph (reduce conj (:vertices the-graph) new-vertices) (:edges the-graph)))


(defn add-edges
  "Add one or more edges to the specified graph, f.i.

  `(add-edges my-graph #{:a :b} #{:b :c})`

  Will return a copy of the specified graph with the new edges. Will silently ignore
  every edge that is not a set of two elements"
  [the-graph & the-edges]
  (let [filtered-edges (filter valid-edge? the-edges)]
    (graph (:vertices the-graph) (reduce conj (:edges the-graph) filtered-edges))))

(defn remove-edges
  [the-graph & the-edges]
  (graph (:vertices the-graph) (reduce disj (:edges the-graph) the-edges)))
