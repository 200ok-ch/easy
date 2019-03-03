(ns easy.revenue
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.templating :as templating]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.revenue.item :as item]
            [easy.customers :as customers]
            [clojure.string :refer [join replace split]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; ------------------------------------------------------------
;; spec

(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))
(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))

;; required
(s/def ::type #{"revenue"})
(s/def ::date util/date?)
(s/def ::customer-id pos-int?)
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
(s/def ::latex-content string?)
(s/def ::latex-directory string?)
(s/def ::latex-filename string?)
(s/def ::pdflatex-cmd string?)

(s/def ::event (s/keys :req-un [::type
                                ::date
                                ::customer-id
                                ::number
                                ::version
                                ::items]
                       :opt-un [::customers/customer
                                ::settled
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
                                ::latex-template
                                ::latex-content
                                ::latex-directory
                                ::latex-filename
                                ::pdflatex-cmd]))

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

(defn lookup-customer [{id :customer-id :as event}]
  (->> @config
       :customers
       (filter #(= id (:number %)))
       first
       (assoc* event :customer)))

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

(defn transform-items [revenue]
  (update revenue :items (partial map item/transform)))

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
  (->> [:customer-id :number :version]
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
      (assoc* :report-template
              (get-in @config [:invoice :report :template]))
      (assoc* :latex-template
              (get-in @config [:invoice :latex :template]))
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :revenue]))))

(defn order-items-by-amount [revenue]
  (merge revenue
         {:items (reverse (sort-by :amount (:items revenue)))}))

(defn add-latex-content [revenue]
  (->> revenue
       templating/render-latex
       (assoc* revenue :latex-content)))

(defn add-latex-directory [revenue]
  (let [directory (get-in @config [:invoice :latex :directory])]
    (->> (templating/template directory revenue)
         (assoc* revenue :latex-directory))))

(defn add-latex-filename [revenue]
  (let [filename (get-in @config [:invoice :latex :filename])]
    (->> (templating/template filename revenue)
         (assoc* revenue :latex-filename))))

;; TODO refactor everything so that latex has its own submap wuth
;; content, path etc. so we can have a generic write! function which
;; takes a revenue and a key to the submap, see commented function
;; below.
(defn write-latex! [{directory :latex-directory
                     filename :latex-filename
                     content :latex-content
                     :as revenue}]
  ;; TODO use some path join here
  (-> (str directory "/" filename)
      (util/spit content))
  revenue)

;; (defn write! [event format]
;;   (->> [:path :content]
;;        (map (format event))
;;        (apply util/spit)))

(defn add-pdflatex-cmd [{directory :latex-directory
                         filename :latex-filename
                         :as revenue}]
  (->> (str "(cd " directory " && pdflatex " filename ")")
       (assoc* revenue :pdflatex-cmd)))

(defn run-pdflatex! [{cmd :pdflatex-cmd
                      :as revenue}]
  (util/sh cmd)
  revenue)

(defn transform-latex! [revenue]
  (-> revenue
      order-items-by-amount
      add-latex-content
      add-latex-directory
      add-latex-filename
      write-latex!
      add-pdflatex-cmd
      run-pdflatex!
      ;; TODO run xdg-open on the pdf file
      ))

(defmethod transform :revenue [event]
  (-> event
      (common/validate! ::event)
      merge-defaults
      lookup-customer
      common/add-iso-date
      add-iso-settled
      add-period
      add-ledger-state
      add-tax-rate-in
      add-tax-rate-out
      add-tax-period
      transform-items
      add-net-total
      add-tax-in
      add-gross-total
      add-tax-out
      add-tax-win
      add-invoice-no
      add-templates
      (common/validate! ::event)))
