(ns easy.transform
  "Transform has to has its own namespace to avoid cyclic dependencies.")

(defmulti transform
  "Events will be transformed based on their type."
  (comp keyword :type))
