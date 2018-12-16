(ns easy.common
  "This namespace covers the common requirements for all events."
  (:require [cljs.spec.alpha :as s]
            [easy.util :refer [date?]]))

(s/def ::type #{"revenue" ; Ertrag
                "expense"}) ; Aufwand

(s/def ::date date?)

(s/def ::event (s/keys :req-un [::type
                                ::date]))

(s/def ::events (s/coll-of ::event))
