(ns easy.redistribution
  "An *redistribution* example:
  ```
  - type: redistribution
    date: 2018-12-31
    accounts:
      - payer: employee1
        amount: 10000
      - payer: employee2
        amount: 50000
  ```"
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.redistribution.account :as account]))


;; spec


(s/def ::type #{"redistribution"})
(s/def ::date util/date?)
(s/def ::accounts (s/coll-of ::account/account))

(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::accounts]
                       :opt-un [::iso-date
                                ::ledger-template]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; helpers


(defn- transform-accounts [evt]
  (update evt :accounts (partial map #(account/transform % evt))))


;; transformers


(defn add-amount [evt]
  (let [div #(/ % (-> evt :accounts count))]
    (->> evt
         :accounts ;; => list of accounts
         (map :redistribution-amount) ;; => list of contributions
         (reduce +) ;; => sum
         div ;; => one share
         util/round-currency
         (assoc* evt :amount))))


(defmethod transform :redistribution [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      transform-accounts
      add-amount
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :redistribution]))
      (common/validate! ::event)))
