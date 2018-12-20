(ns easy.revenue
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.revenue.item :as item]
            [clojure.string :refer [join]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; ------------------------------------------------------------
;; spec

(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))
(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))

;; required
(s/def ::type #{"revenue"})
(s/def ::date util/date?)
(s/def ::customer pos-int?)
(s/def ::number pos-int?) ;; sequence
(s/def ::version pos-int?)
(s/def ::items (s/coll-of ::item/item))

;; optional
(s/def ::settled util/date?)
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::iso-settled (s/and string? common/match-iso-date))
(s/def ::deadline pos-int?) ;; in days
(s/def ::header string?)
(s/def ::footer string?)
(s/def ::tax-rate-in float?)
(s/def ::tax-rate-out float?)
(s/def ::tax-in float?)
(s/def ::tax-out float?)
(s/def ::tax-win float?)
(s/def ::net-total float?)
(s/def ::gross-total float?)
(s/def ::invoice-no (s/and string? match-invoice-no))
(s/def ::tax-period (s/or :settled (s/and string? match-period)
                          :unsettled #{"Unsettled"}))
(s/def ::period (s/or :settled (s/and string? match-period)
                      :unsettled #{"Unsettled"}))
(s/def ::ledger-state #{"!" "*"})
(s/def ::ledger-template (s/and string? common/match-template))
(s/def ::latex-template (s/and string? common/match-template))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::customer
                                ::number
                                ::version
                                ::items]
                       :opt-un [::settled
                                ::deadline
                                ::header
                                ::footer
                                ::iso-date
                                ::iso-settled
                                ::tax-rate-in
                                ::tax-rate-out
                                ::tax-in
                                ::tax-out
                                ::tax-win
                                ::net-total
                                ::gross-total
                                ::invoice-no
                                ::tax-period
                                ::period
                                ::ledger-state
                                ::ledger-template
                                ::latex-template]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {:deadline 30})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

;; TODO add doc strings to all functions
;; TODO add pre conditions to all functions

(defn add-iso-settled [event]
  (if-let [settled (:settled event)]
    (->> settled
         cljs-time/date-time
         (time/unparse util/iso-formatter)
         (assoc* event :iso-settled))
    event))

;; TODO make the tax rate configurable via config
(defn tax-rate-in [revenue]
  (if (< (:date revenue)
         (time/parse "2018-01-01"))
    0.08
    0.077))

;; TODO make the tax rate configurable via config
(defn tax-rate-out [revenue]
  (let [date (:date revenue)]
    (cond
      ;; saldo pre 2018
      (< date (time/parse "2018-01-01")) 0.061
      ;; TODO maybe, because we switched to effective
      ;; (> date (time/parse "2018-12-31")) 0.77
      ;; saldo from 2018
      :else 0.059)))

(defn add-tax-rate-in [revenue]
  (->> (tax-rate-in revenue)
       (assoc* revenue :tax-rate-in)))

(defn add-tax-rate-out [revenue]
  (->> (tax-rate-out revenue)
       (assoc* revenue :tax-rate-out)))

(defn add-tax-in [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-in revenue))
       util/round-currency
       (assoc* revenue :tax-in)))

(defn add-tax-out [revenue]
  (->> (:gross-total revenue)
       (* (:tax-rate-out revenue))
       util/round-currency
       (assoc* revenue :tax-out)))

(defn add-tax-win [revenue]
  (->> (:tax-out revenue)
       (- (:tax-in revenue))
       util/round-currency
       (assoc* revenue :tax-win)))

;; TODO move to revenue.item
(defn add-item-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       ;; TODO calculate and subtract discount
       util/round-currency
       (assoc* item :amount)))

;; TODO maybe move to revenue.item
(defn add-items-amount [revenue]
  (update revenue :items #(map add-item-amount %)))

(defn add-net-total [revenue]
  (->> revenue
       :items
       (map :amount)
       (reduce +)
       ;; TODO calculate and subtract discount
       util/round-currency
       (assoc* revenue :net-total)))

(defn add-gross-total [revenue]
  (->> (+ (:net-total revenue)
          (:tax-in revenue))
       util/round-currency
       (assoc* revenue :gross-total)))

(defn add-invoice-no [revenue]
  (->> [:customer :number :version]
       (map revenue)
       (join ".")
       (assoc* revenue :invoice-no)))

;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-tax-period [revenue]
  (->> (if-let [date (:settled revenue)]
         (cond
           (and (>= date (time/parse "2018-01-01"))
                (<= date (time/parse "2018-05-31")))
           "2018-H1"
           (and (>= date (time/parse "2018-06-01"))
                (<= date (time/parse "2018-12-31")))
           "2018-H2"
           (and (>= date (time/parse "2019-01-01"))
                (<= date (time/parse "2019-05-31")))
           "2019-H1"
           (and (>= date (time/parse "2019-06-01"))
                (<= date (time/parse "2019-12-31")))
           "2019-H2"
           :else "Unknown")
         "Unsettled")
       (assoc* revenue :tax-period)))

(defn add-ledger-state
  "Sets `:ledger-state` to either `*` or `!`, depending on the presence
  of `:settled`"
  [revenue]
  (->> (if (:settled revenue) "*" "!")
       (assoc* revenue :ledger-state)))

;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-period [revenue]
  (->> (let [date (:date revenue)]
         (cond
           (and (>= date (time/parse "2017-06-01"))
                (<= date (time/parse "2017-12-31")))
           "2017-H2"
           (and (>= date (time/parse "2018-01-01"))
                (<= date (time/parse "2018-05-31")))
           "2018-H1"
           (and (>= date (time/parse "2018-06-01"))
                (<= date (time/parse "2018-12-31")))
           "2018-H2"
           (and (>= date (time/parse "2019-01-01"))
                (<= date (time/parse "2019-05-31")))
           "2019-H1"
           (and (>= date (time/parse "2019-06-01"))
                (<= date (time/parse "2019-12-31")))
           "2019-H2"
           :else "Unknown"))
       (assoc* revenue :period)))

(defn add-templates [revenue]
  (-> revenue
      (assoc* :latex-template
              (get-in @config [:templates :latex :invoice]))
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :revenue]))))

(defmethod transform :revenue [event]
  (-> event
      (common/validate! ::event)
      merge-defaults
      ;; TODO item/merge-defaults
      common/add-iso-date
      add-iso-settled
      add-period
      add-ledger-state
      add-tax-rate-in
      add-tax-rate-out
      add-tax-period
      ;; TODO read-timesheets
      ;; TODO add-items-hours
      add-items-amount
      add-net-total
      add-tax-in
      add-gross-total
      add-tax-out
      add-tax-win
      add-invoice-no
      ;; TODO add-invoice-settings, e.g. phil, alain, neutral
      ;; TODO add-customer-number, e.g. 2017-4
      ;; TODO add-customer-address
      add-templates
      (common/validate! ::event)))
