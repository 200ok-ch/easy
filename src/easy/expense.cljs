(ns easy.expense
  "An *expense* example:
  ```
  - type: expense
    account: Aufwand:6940-Bankspesen
    payer: Joint
    amount: 60
    date: 2018-01-31
    description: Bankgebühren
  ```"
  (:require [cljs.spec.alpha :as s]
            [cljs-time.format :as time]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.common.tax :as tax]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))


;; spec - required
(s/def ::type #{"expense"})
(s/def ::date util/date?)
(s/def ::account string?)
(s/def ::payer string?)
(s/def ::amount float?)

;; spec - optional
(s/def ::description string?)
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::payer
                                ::account]
                       :opt-un [::description
                                ::ledger-template]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; transformers


(defn- add-respect-tax-rate [evt]
  (->> (tax/lookup-rate :respect-tax-rate evt)
       (assoc* evt :respect-tax-rate)))


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
      tax/add-period
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :expense]))
      (common/validate! ::event)))
