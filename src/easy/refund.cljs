;; TODO this is just a copy

(ns easy.refund
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))

;; ------------------------------------------------------------
;; spec

;; required
(s/def ::type #{"refund"})
(s/def ::date util/date?)
(s/def ::amount float?)
(s/def ::beneficiary string?)

;; optional
(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount]
                       :opt-un [::description
                                ::beneficiary]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

(defmethod transform :refund [event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :refund]))
      (common/validate! ::event)))
