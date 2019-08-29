(ns easy.adminshare
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.adminshare.account :as account]))


;; spec


;; required
(s/def ::type #{"adminshare"})
(s/def ::date util/date?)
(s/def ::accounts (s/coll-of ::account/account))


;; optional
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::accounts]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; transformer

(defn transform-accounts [evt]
  (update evt :accounts (partial map account/transform)))


(defn add-amount [evt]
  (let [div #(/ % (-> evt :accounts count))]
    (->> evt
         :accounts
         (map :adminshare-amount)
         (reduce +)
         div
         util/round-currency
         (assoc* evt :amount))))


(defmethod transform :adminshare [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      transform-accounts
      add-amount
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :adminshare]))
      (common/validate! ::event)))
