(ns easy.salary.items
  (:require [cljs.spec.alpha :as s]))

(s/def ::beneficiary string?)
(s/def ::amount float?)

(s/def ::item (s/keys :req-un [::beneficiary
                               ::amount]))

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))
