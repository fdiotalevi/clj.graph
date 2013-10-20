(ns clj.graph)

(defrecord Graph [vertices edges])

(defn graph [vertices edges] (->Graph vertices edges))
