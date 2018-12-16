(ns easy.expense
  (:require [cljs.spec.alpha :as s]
            [easy.util :refer [date?]]))

(s/def ::type (partial = "expense"))

(s/def ::date date?)

(s/def ::amount float?)

(s/def ::beneficiary string?)

(s/def ::description string?)

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::beneficiary]
                       :opt-un [::description]))

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))
