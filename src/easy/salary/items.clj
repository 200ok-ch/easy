(ns easy.salary.items
  (:require [clojure.spec.alpha :as s]))

;;; spec

(s/def ::beneficiary string?)
(s/def ::amount number?)

(s/def ::item (s/keys :req-un [::beneficiary
                               ::amount]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))
