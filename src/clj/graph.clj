(ns clj.graph)

(defrecord Graph [vertices edges])

(defn- valid-edges?
  ([edges]
     (if (empty? edges)
       true
       (let [valids (map #(and (set? %) (= 2 (count %))) edges)]
         (reduce #(and %1 %2) valids)))))

(defn graph [vertices edges]
  (if (and (set? vertices) (valid-edges? edges))
    (->Graph vertices edges)
    (throw (Exception. "Invalid graph"))))

