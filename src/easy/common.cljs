(ns easy.common
  "This namespace covers the common requirements for all events."
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))


;; spec


(def match-iso-date (partial re-matches #"^\d{4}-\d\d-\d\d$"))
(def match-template (partial re-matches #".*\.hbs"))

;; NOTE: This is a set of all known event types. New event types need
;; to be listed here otherwise they will result in an error. This is
;; an early guard against typos in the name of event types -
;; unfortunately not much more, so we might as well drop it entirely.
(s/def ::type #{"plain"            ;; Allgemeine Buchung
                "opening"          ;; Eröffnungsbilanz
                "invoice"          ;; Rechnung wurde gestellt
                "settlement"       ;; Rechnung wurde beglichen
                "expense"          ;; Ausgabe
                "refund"           ;; Rückerstattung
                "reconciliation"   ;; Ausgleichsbuchung
                "salary"           ;; Gehalt
                "outlay"           ;; Spesenabrechnung
                "redistribution"}) ;; "200ok Sozialfaktor"

(s/def ::date (s/or :date util/date?
                    :iso-string (s/and string? match-iso-date)))
(s/def ::property string?)
(s/def ::ignore-warnings (s/coll-of ::property))
(s/def ::file util/file-exists?)
(s/def ::event (s/keys :req-un [::type
                                ::date]
                       :opt-un [::ignore-warnings
                                ::file]))
(s/def ::events (s/coll-of ::event))


;; helpers


(defn ignore-warning?
  "Checks if `evt` has `:ignore-warnings` set for `key`."
  [evt key]
  (->> (get evt :ignore-warnings [])
       (util/include? (name key))))


(defn harmonize [evt]
  (->> evt
       (util/harmonize-date-field :date)
       ;; TODO: remove, we don' use `:settled` anymore
       (util/harmonize-date-field :settled)))


(defn validate!
  "Same as `util/validate!`, but with arguments swapped."
  [x spec]
  (util/validate! spec x))


(defn make-iso-date [date]
  (->> date
       cljs-time/date-time
       (time/unparse util/iso-formatter)))


;; transformers

(defn add-iso-date
  "Transformer that takes the value of `:date`, builds an iso-date
  string from it and assoc's it as `:iso-date`."
  [evt]
  (->> evt
       :date
       make-iso-date
       (assoc* evt :iso-date)))
