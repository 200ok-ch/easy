(ns easy.plain
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))


;; spec


;; required
(s/def ::type #{"plain"})
(s/def ::date util/date?)
(s/def ::amount float?)
(s/def ::source string?)
(s/def ::target string?)


;; optional
(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))


(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::amount
                                ::source
                                ::target
                                ::description]
                       :opt-un [::iso-date
                                ::ledger-template]))


;; defaults


(def defaults
  {})


(def merge-defaults
  (partial merge defaults))


;; transformer


(defmethod transform :plain [_ event]
  (-> event
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :plain]))
      (common/validate! ::event)))
