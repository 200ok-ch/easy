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
(s/def ::payer string?)
(s/def ::account string?)


;; optional
(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))


(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::payer
                                ::account]
                       :opt-un [::description]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; transformer


(defn- add-respect-tax-rate [evt]
  ;; TODO unhardcode
  (assoc* evt :respect-tax-rate 0.077))


(defn- add-respect-tax-amount [evt]
  (->> (* (:foreign-amount evt)
          (:exchange-rate evt)
          (:respect-tax-rate evt))
       util/round-currency
       (assoc* evt :respect-tax-amount)))


(defmethod transform :expense [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      add-respect-tax-rate
      add-respect-tax-amount
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :expense]))
      (common/validate! ::event)))
