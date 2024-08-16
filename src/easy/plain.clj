(ns easy.plain
  "A plain event is the most generic kind of booking.

  A *plain* example:
  ```
  - type: plain
    date: 2018-12-31
    source: Aufwand:6799-Durchlaufkonto-Spesen
    target: Aufwand:6000-Raumaufwand
    amount: 49.95
    description: Spesen-Sammelbuchung Miete
  ```"
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]))

;;; spec

(s/def ::type #{"plain"})
(s/def ::date util/date?)
(s/def ::source string?)
(s/def ::target string?)
(s/def ::amount number?)

(s/def ::description string?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::source
                                ::target
                                ::amount]
                       :opt-un [::iso-date
                                ::description
                                ::ledger-template]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;;; transformers

(defmethod transform :plain [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :plain]))
      (common/validate! ::event)))
