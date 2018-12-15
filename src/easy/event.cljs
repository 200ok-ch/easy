(ns easy.event
  (:require [cljs.spec.alpha :as s]
            [easy.util :refer [date?]]))

(s/def ::type #{"revenue" ; Ertrag
                "expense"}) ; Aufwand

(s/def ::date date?)

(s/def ::customer int?)

(s/def ::number int?)

(s/def ::version int?)

(s/def ::deadline (s/or ::date int?))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::customer
                                ::number
                                ::version
                                :easy.item/items]
                       :opt-un [::deadline]))

(s/def ::events (s/coll-of ::event))

(def defaults
  {:deadline 30})

(def merge-defaults
  (partial merge defaults))
