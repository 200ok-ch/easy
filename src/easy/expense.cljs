(ns easy.expense
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))


;; spec


;; required
(s/def ::type #{"expense"})
(s/def ::date util/date?)
(s/def ::amount float?)
(s/def ::category string?)
(s/def ::account string?)


;; optional
(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))


(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::category
                                ::account]
                       :opt-un [::description]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; transformer

(defn- add-respect-tax-amount [evt]
  (assoc* evt :respect-tax-amount (util/round-currency (* (:amount evt) 0.077))))


(defmethod transform :expense [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      add-respect-tax-amount
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :expense]))
      (common/validate! ::event)))
