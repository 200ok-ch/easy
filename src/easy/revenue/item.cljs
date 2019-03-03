(ns easy.revenue.item
  (:require [cljs.spec.alpha :as s]))

(s/def ::rate float?)

(s/def ::hours float?)

(s/def ::beneficiary string?)

(s/def ::discount float?)

(s/def ::timesheet (s/and string? #(re-matches #"\.csv$" %)))

;; TODO how do I spec: either hours or timesheet has to be present?

(s/def ::item (s/keys :req-un [::rate
                               ::beneficiary]
                      :opt-un [::hours
                               ::timesheet
                               ::timesheet-data
                               ::discount
                               ::amount]))

(def defaults
  {:discount 0})

(def merge-defaults
  (partial merge defaults))
