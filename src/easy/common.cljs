(ns easy.common
  "This namespace covers the common requirements for all events."
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; ------------------------------------------------------------
;; spec

(def match-iso-date (partial re-matches #"^\d{4}-\d\d-\d\d$"))
(def match-template (partial re-matches #"\.hbs$"))

;; required
(s/def ::type #{"revenue" "expense"})
(s/def ::date util/date?)

(s/def ::event (s/keys :req-un [::type
                                ::date]))

(s/def ::events (s/coll-of ::event))

;; ------------------------------------------------------------
;; transformer

(defn add-iso-date [event]
  (->> event
       :date
       cljs-time/date-time
       (time/unparse util/iso-formatter)
       (assoc* event :iso-date)))
