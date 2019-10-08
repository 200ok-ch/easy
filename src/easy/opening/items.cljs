(ns easy.opening.items
  (:require [cljs.spec.alpha :as s]))


;; spec


(s/def ::account string?)
(s/def ::amount float?)

(s/def ::item (s/keys :req-un [::account
                               ::amount]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))
