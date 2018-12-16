(ns easy.revenue.item
  (:require [cljs.spec.alpha :as s]))

(s/def ::rate float?)

(s/def ::hours float?)

(s/def ::beneficiary string?)

(s/def ::discount float?)

(s/def ::timesheet (s/and string? #(re-matches #"\.csv$" %)))

(s/def ::item (s/keys :req-un [::rate
                               ::hours
                               ::beneficiary]
                      :opt-un [::timesheet
                               ::discount]))

(def defaults
  {:discount 0})

(def merge-defaults
  (partial merge defaults))
