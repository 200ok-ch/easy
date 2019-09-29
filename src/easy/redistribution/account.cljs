(ns easy.redistribution.account
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]))


;; spec - required
(s/def ::payer string?)
(s/def ::amount float?)

;; spec - optional
(s/def ::redistribution-factor float?)
(s/def ::redistribution-amount float?)

(s/def ::account (s/keys :req-un [::payer
                                  ::amount]
                         :opt-un [::redistribution-factor
                                  ::redistribution-amount]))


;; defaults


(def defaults
  {})

(def ^:private merge-defaults
  (partial merge defaults))


;; helpers


(defn- calculate-amount [account]
  (* (account :amount)
     (account :redistribution-factor)))


;; transformers


(defn- add-redistribution-factor
  [account {:keys [redistribution-factor] :or {redistribution-factor 0.1}}]
  (assoc* account :redistribution-factor redistribution-factor))


(defn- add-redistribution-amount [account]
  (->> account
       calculate-amount
       util/round-currency
       (assoc* account :redistribution-amount)))


(defn transform [account evt]
  (-> account
      merge-defaults
      (add-redistribution-factor evt)
      add-redistribution-amount))
