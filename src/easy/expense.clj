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
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.common.tax :as tax]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [clj-time.core :as time]))

;;; spec

(s/def ::type #{"expense"})
(s/def ::date util/date?)
(s/def ::account string?)
(s/def ::payer string?)
(s/def ::amount number?)

(s/def ::description string?)
(s/def ::addendum (s/or :string string?
                        :number number?))
(s/def ::description-with-addendum string?)
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::payer
                                ::account]
                       :opt-un [::description
                                ::addendum
                                ::description-with-addendum
                                ::ledger-template]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;;; transformers

(defn- add-respect-tax-rate [evt]
  (->> (tax/lookup-rate :respect-tax-rate evt)
       (assoc* evt :respect-tax-rate)))

(defn- add-respect-tax-amount [evt]
  (->> (* (:foreign-amount evt)
          (:exchange-rate evt)
          (:respect-tax-rate evt))
       util/round-currency
       (assoc* evt :respect-tax-amount)))

(defn- add-description-with-addendum [evt]
  (->> [:description :addendum]
       (map evt)
       (remove nil?)
       (str/join " ")
       (assoc* evt :description-with-addendum)))

(defn add-deferral [evt]
  (assoc* evt :deferral
          (if-let [invoice-date (-> evt :invoice-date)]
            (not= (-> evt :date time/year)
                  (-> invoice-date time/year))
            false)))

(defmethod transform :expense [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      add-respect-tax-rate
      add-respect-tax-amount
      tax/add-period
      add-deferral
      add-description-with-addendum
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :expense]))
      (common/validate! ::event)))
