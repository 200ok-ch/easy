(ns easy.opening.items
  (:require [clojure.spec.alpha :as s]))

;;; spec

(s/def ::account string?)
(s/def ::amount number?)

(s/def ::item (s/keys :req-un [::account
                               ::amount]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))
