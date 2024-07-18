(ns easy.outlay.detail
  "A *detail* example:
  ```
  - amount: 205.10
    account: Aktiva:6500-Büromaterial
    addendum: Büromaterial
  ```"
  (:require [clojure.spec.alpha :as s]))

;;; spec

(s/def ::amount number?)
(s/def ::account string?)
(s/def ::addendum string?)

(s/def ::detail (s/keys :req-un [::amount
                                 ::account]
                        :opt-un [::addendum]))
