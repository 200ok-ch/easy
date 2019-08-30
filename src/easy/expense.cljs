(ns easy.expense
  (:require [cljs.spec.alpha :as s]
            [cljs-time.format :as time]
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

;; TODO refactor into a tax namespace, settlement has the same code
;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-tax-period
  "The tax-period is when the vat is due."
  [evt]
  (->> (let [date (-> evt :date)]
         (cond
           (and (>= date (time/parse "2017-06-01"))
                (<= date (time/parse "2017-12-31")))
           "2017-H2"
           (and (>= date (time/parse "2018-01-01"))
                (<= date (time/parse "2018-05-31")))
           "2018-H1"
           (and (>= date (time/parse "2018-06-01"))
                (<= date (time/parse "2018-12-31")))
           "2018-H2"
           (and (>= date (time/parse "2019-01-01"))
                (<= date (time/parse "2019-05-31")))
           "2019-H1"
           (and (>= date (time/parse "2019-06-01"))
                (<= date (time/parse "2019-12-31")))
           "2019-H2"
           :else "Unknown"))
       (assoc* evt :tax-period)))


(defmethod transform :expense [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      add-respect-tax-rate
      add-respect-tax-amount
      add-tax-period
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :expense]))
      (common/validate! ::event)))
