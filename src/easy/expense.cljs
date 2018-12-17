(ns easy.expense
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))

;; ------------------------------------------------------------
;; spec

(s/def ::type (partial = "expense"))

(s/def ::date util/date?)

(s/def ::amount float?)

(s/def ::beneficiary string?)

(s/def ::description string?)

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::beneficiary]
                       :opt-un [::description]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

(defmethod transform :expense [event]
  (if (s/valid? ::event event)
    (-> event
        common/add-iso-date
        (assoc :ledger-state "*") ;; always cleared
        (common/add-ledger-template
         (get-in @config [:templates :expense])))
    ;; else explain
    (s/explain ::event event)))
