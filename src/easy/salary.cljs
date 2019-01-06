(ns easy.salary
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.salary.items :as items]))

;; ------------------------------------------------------------
;; spec

;; required
(s/def ::type #{"salary"})
(s/def ::date util/date?)
(s/def ::items (s/coll-of ::items/item))

;; optional
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::items]
                       :opt-un [::iso-date
                                ::ledger-template]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

(defmethod transform :salary [event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :salary]))
      (common/validate! ::event)))
