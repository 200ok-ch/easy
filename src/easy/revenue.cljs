(ns easy.revenue
  (:require [cljs.spec.alpha :as s]
            [easy.util :refer [date?]]
            [easy.revenue.item :as item]))

(s/def ::type (partial = "revenue"))

(s/def ::date date?)

(s/def ::customer int?)

(s/def ::number int?)

(s/def ::version int?)

(s/def ::deadline (s/or ::date int?))

(s/def ::items (s/coll-of ::item/item))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::customer
                                ::number
                                ::version
                                ::items]
                       :opt-un [::deadline]))

(def defaults
  {:deadline 30})

(def merge-defaults
  (partial merge defaults))
