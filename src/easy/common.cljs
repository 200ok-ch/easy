(ns easy.common
  "This namespace covers the common requirements for all events."
  (:require [cljs.spec.alpha :as s]
            ;; from this code base
            [easy.util :as util]
            ;; via clojars/maven
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; ------------------------------------------------------------
;; spec

(s/def ::type #{"revenue"
                "expense"})

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
       (assoc event :iso-date)))

(defn add-ledger-template [revenue path]
  (assoc revenue :ledger-template path))
