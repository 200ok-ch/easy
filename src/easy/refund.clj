(ns easy.refund
  "A *refund* example:
  ```
  - type: refund
    date: 2018-08-22
    description: Rückz. Direkte Bundesst. 2016
    amount: 1234.56
    source: 8900-Direkte-Steuern
  ```"
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))

;;; spec

(s/def ::type #{"refund"})
(s/def ::date util/date?)
(s/def ::amount number?)
(s/def ::beneficiary string?)

(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-state #{"*"})
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount]
                       :opt-un [::description
                                ::beneficiary]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;;; transformers

(defmethod transform :refund [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-state "*") ;; always cleared
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :refund]))
      (common/validate! ::event)))
