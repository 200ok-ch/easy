(ns easy.opening
  "The begining of an *opening* event example:
  ```
  - type: opening
    date: 2018-01-01
    items:
      - account: 'Aktiva:1010-Postfinance'
        amount: 1234.12
      - account: 'Aktiva:1100-Forderungen-aus-Lieferungen-und-Leistungen'
        amount: 4321.21
      - account: 'Aktiva:1109-Wertberichtigungen-FLL'
        amount: -2340
  ...
  ```"
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.opening.items :as items]))

;;; spec

(s/def ::type #{"opening"})
(s/def ::date util/date?) ;; TODO: opening should always be on 1st of January
(s/def ::items (s/coll-of ::items/item))

(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::ledger-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::items]
                       :opt-un [::iso-date
                                ::ledger-template]))

;;; defaults

(def defaults
  {})

(def merge-defaults
  (partial merge defaults))

;;; transformers

(defmethod transform :opening [_ evt]
  (-> evt
      (common/validate! ::event)
      common/add-iso-date
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :opening]))
      (common/validate! ::event)))
