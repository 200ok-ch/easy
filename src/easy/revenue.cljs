(ns easy.revenue
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util]
            [easy.common :as common]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.revenue.item :as item]
            [clojure.string :refer [join]]
            ;; via clojars/maven
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))


;; ------------------------------------------------------------
;; spec

(s/def ::type (partial = "revenue"))

(s/def ::date util/date?)

(s/def ::settled util/date?)

(s/def ::customer int?)

(s/def ::number int?)

(s/def ::version int?)

(s/def ::deadline (s/or ::date int?))

(s/def ::items (s/coll-of ::item/item))

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::customer
                                ::number
                                ::version
                                ::items]
                       :opt-un [::settled
                                ::deadline]))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {:deadline 30})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; transformer

(defn add-iso-settled [event]
  (if-let [settled (:settled event)]
    (->> settled
         cljs-time/date-time
         (time/unparse util/iso-formatter)
         (assoc event :iso-settled))
    event))

(defn tax-rate-in [revenue]
  (if (< (:date revenue)
         (time/parse "2018-01-01"))
    0.08
    0.077))

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
       (assoc revenue :tax-rate-in)))

(defn add-tax-rate-out [revenue]
  (->> (tax-rate-out revenue)
       (assoc revenue :tax-rate-out)))

(defn add-tax-in [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-in revenue))
       util/round-currency
       (assoc revenue :tax-in)))

(defn add-tax-out [revenue]
  (->> (:gross-total revenue)
       (* (:tax-rate-out revenue))
       util/round-currency
       (assoc revenue :tax-out)))

(defn add-tax-win [revenue]
  (->> (:tax-out revenue)
       (- (:tax-in revenue))
       util/round-currency
       (assoc revenue :tax-win)))

(defn add-item-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       ;; TODO calculate and subtract discount
       util/round-currency
       (assoc item :amount)))

(defn add-items-amount [revenue]
  (update revenue :items #(map add-item-amount %)))

(defn add-net-total [revenue]
  (->> revenue
       :items
       (map :amount)
       (reduce +)
       ;; TODO calculate and subtract discount
       util/round-currency
       (assoc revenue :net-total)))

(defn add-gross-total [revenue]
  (->> (+ (:net-total revenue)
          (:tax-in revenue))
       util/round-currency
       (assoc revenue :gross-total)))

(defn add-invoice-no [revenue]
  (->> [:customer :number :version]
       (map revenue)
       (join ".")
       (assoc revenue :invoice-no)))

;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-tax-period [revenue]
  (->> (if-let [date (:settled revenue)]
         (cond
           (and (>= date (time/parse "2018-01-01"))
                (<= date (time/parse "2018-05-31")))
           "2018-S1"
           (and (>= date (time/parse "2018-06-01"))
                (<= date (time/parse "2018-12-31")))
           "2018-S2"
           (and (>= date (time/parse "2019-01-01"))
                (<= date (time/parse "2019-05-31")))
           "2019-S1"
           (and (>= date (time/parse "2019-06-01"))
                (<= date (time/parse "2019-12-31")))
           "2019-S2"
           :else "Unknown")
         "Unsettled")
       (assoc revenue :tax-period)))

(defn add-ledger-state [revenue]
  (->> (if (:settled revenue) "*" "!")
       (assoc revenue :ledger-state)))

(defn add-period [revenue]
  (->> (let [date (:date revenue)]
         (cond
           (and (>= date (time/parse "2017-06-01"))
                (<= date (time/parse "2017-12-31")))
           "2017-S2"
           (and (>= date (time/parse "2018-01-01"))
                (<= date (time/parse "2018-05-31")))
           "2018-S1"
           (and (>= date (time/parse "2018-06-01"))
                (<= date (time/parse "2018-12-31")))
           "2018-S2"
           (and (>= date (time/parse "2019-01-01"))
                (<= date (time/parse "2019-05-31")))
           "2019-S1"
           (and (>= date (time/parse "2019-06-01"))
                (<= date (time/parse "2019-12-31")))
           "2019-S2"
           :else "Unknown"))
       (assoc revenue :period)))

(defmethod transform :revenue [event]
  (if (s/valid? ::event event)
    (-> event
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
        (common/add-ledger-template
         (get-in @config [:templates :revenue])))
    ;; else explain
    (s/explain ::event event)))
