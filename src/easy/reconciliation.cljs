;; TODO this is just a copy

(ns easy.reconciliation
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))

;; ------------------------------------------------------------
;; spec

;; required
(s/def ::type #{"reconciliation"})
(s/def ::date util/date?)
(s/def ::amount float?)
(s/def ::account string?)

;; optional
(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::account]
                       :opt-un [::description]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

(defmethod transform :reconciliation [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :reconciliation]))
      (common/validate! ::event)))
