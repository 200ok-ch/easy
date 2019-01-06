(ns easy.opening
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.opening.items :as items]))

;; ------------------------------------------------------------
;; spec

;; required
(s/def ::type #{"opening"})
;; TODO opening should always be on 1st of January
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

(defmethod transform :opening [event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :opening]))
      (common/validate! ::event)))
