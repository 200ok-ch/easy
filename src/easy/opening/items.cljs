(ns easy.opening.items
  (:require [cljs.spec.alpha :as s]))

(s/def ::account string?)
(s/def ::amount float?)

(s/def ::item (s/keys :req-un [::account
                               ::amount]))

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))
