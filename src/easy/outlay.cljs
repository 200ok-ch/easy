(ns easy.outlay
  "An *outlay* example:
  ```
  - type: outlay
    description: Spesen 2018
    beneficiary: employee1
    date: 2018-12-31
    amount: 2919.79
    receipt: 10
  ```"
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))


;; spec


(s/def ::type #{"outlay"})
(s/def ::date util/date?)
(s/def ::amount float?)
(s/def ::beneficiary string?)

(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::beneficiary]
                       :opt-un [::description]))


;; defaults


(def defaults
  {})

(def merge-defaults
  (partial merge defaults))


;; transformers


(defmethod transform :outlay [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :outlay]))
      (common/validate! ::event)))
