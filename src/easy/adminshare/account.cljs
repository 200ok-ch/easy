(ns easy.adminshare.account
  (:require [cljs.spec.alpha :as s]
            [testdouble.cljs.csv :as csv]
            [easy.util :as util :refer [assoc*]]))


(s/def ::payer string?)
(s/def ::amount float?)
(s/def ::adminshare-amount float?)


(s/def ::account (s/keys :req-un [::payer
                                  ::amount]
                         :opt-un [::adminshare-amount]))

(def defaults
  {})


;; ------------------------------------------------------------
;; transforms


(def ^:private merge-defaults
  (partial merge defaults))


(defn- add-adminshare-amount [account]
  (->> account
       :amount
       (* 0.1)
       util/round-currency
       (assoc* account :adminshare-amount)))


(defn transform [account]
  (-> account
      merge-defaults
      add-adminshare-amount))
