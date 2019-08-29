(ns easy.common
  "This namespace covers the common requirements for all events."
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))


;; spec


(def match-iso-date (partial re-matches #"^\d{4}-\d\d-\d\d$"))


(def match-template (partial re-matches #".*\.hbs"))


(s/def ::type #{"plain"          ;; Allgemeine Buchung
                "opening"        ;; Eröffnungsbilanz
                "invoice"        ;; Rechnung wurde gestellt
                "settlement"     ;; Rechnung wurde beglichen
                "expense"        ;; Ausgabe
                "refund"         ;; Rückerstattung
                "reconciliation" ;; Ausgleichsbuchung
                "salary"         ;; Gehalt
                "outlay"         ;; Spesenabrechnung
                "adminshare"})   ;; "200ok Sozialfaktor"


(s/def ::date (s/or :date util/date?
                    :iso-string (s/and string? match-iso-date)))


(s/def ::property string?)


(s/def ::ignore-warnings (s/coll-of ::property))


(s/def ::event (s/keys :req-un [::type
                                ::date]
                       :opt-un [::ignore-warnings]))


(s/def ::events (s/coll-of ::event))


;; transformer


(defn harmonize-date-field [field event]
  (if-let [date (field event)]
    (if (string? date)
      ;; NOTE don't use this, this does not return an instance of Date
      ;; (assoc event field (time/parse util/iso-formatter date))
      (assoc event field (js/Date. date))
      event)
    event))


(defn ignore-warning? [evt key]
  (->> (get evt :ignore-warnings [])
       (util/include? (name key))
       not))


(defn harmonize [event]
  (->> event
       (harmonize-date-field :date)
       (harmonize-date-field :settled)))


(defn validate!
  "Same as `util/validate!`, but with arguments swapped."
  [x spec]
  (util/validate! spec x))


(defn add-iso-date [event]
  (->> event
       :date
       cljs-time/date-time
       (time/unparse util/iso-formatter)
       (assoc* event :iso-date)))
