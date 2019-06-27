(ns easy.invoice
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.common.invoice-no :as invoice-no]
            [easy.templating :as templating]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.invoice.item :as item]
            [easy.customers :as customers]
            [clojure.string :refer [join replace split]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))


;; spec


(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))
(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))


;; required
(s/def ::type #{"invoice"})
(s/def ::date util/date?)
(s/def ::items (s/coll-of ::item/item))


;; optional
(s/def ::iso-date (s/and string? common/match-iso-date))
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

(s/def ::event (s/and
                (s/keys :req-un [::type
                                 ::date
                                 ::customer-id
                                 ::number
                                 ::version
                                 ::items]
                        :opt-un [::customers/customer
                                 ::deadline
                                 ::header
                                 ::footer
                                 ::iso-date
                                 ::tax-rate-in
                                 ::tax-rate-out
                                 ::tax-in
                                 ::tax-out
                                 ::tax-win
                                 ::net-total
                                 ::gross-total
                                 ::tax-period
                                 ::period
                                 ::ledger-state
                                 ::ledger-template
                                 ::latex-template
                                 ::latex-content
                                 ::latex-directory
                                 ::latex-filename
                                 ::pdflatex-cmd])
                ::invoice-no/with))

;; defaults


(def defaults
  {:deadline 30})


(def merge-defaults
  (partial merge defaults))


;; transformer


;; TODO add doc strings to all functions
;; TODO add pre conditions to all functions


(defn lookup-customer [{id :customer-id :as event}]
  (->> @config
       :customers
       (filter #(= id (:number %)))
       first
       (assoc* event :customer)))


(defn resolve-settlement [{:keys [invoice-no] :as evt} ctx]
  (if (nil? ctx)
    evt
    (->> ctx
         (filter #(= invoice-no (:invoice-no %)))
         first
         (transform nil)
         (assoc* evt :settlement))))


;; TODO make the tax rate configurable via config
(defn tax-rate-in [evt]
  (if (< (:date evt)
         (time/parse "2018-01-01"))
    0.08
    0.077))

;; TODO make the tax rate configurable via config
(defn tax-rate-out [evt]
  (let [date (:date evt)]
    (cond
      ;; saldo pre 2018
      (< date (time/parse "2018-01-01")) 0.061
      ;; TODO maybe, because we switched to effective
      ;; (> date (time/parse "2018-12-31")) 0.77
      ;; saldo from 2018
      :else 0.059)))

(defn add-tax-rate-in [evt]
  (->> (tax-rate-in evt)
       (assoc* evt :tax-rate-in)))

(defn add-tax-rate-out [evt]
  (->> (tax-rate-out evt)
       (assoc* evt :tax-rate-out)))

(defn add-tax-in [evt]
  (->> (:net-total evt)
       (* (:tax-rate-in evt))
       util/round-currency
       (assoc* evt :tax-in)))

(defn add-tax-out [evt]
  (->> (:gross-total evt)
       (* (:tax-rate-out evt))
       util/round-currency
       (assoc* evt :tax-out)))

(defn add-tax-win [evt]
  (->> (:tax-out evt)
       (- (:tax-in evt))
       util/round-currency
       (assoc* evt :tax-win)))

(defn transform-items [evt]
  (update evt :items (partial map item/transform)))

(defn add-net-total [evt]
  (->> evt
       :items
       (map :amount)
       (reduce +)
       ;; TODO calculate and subtract discount
       util/round-currency
       (assoc* evt :net-total)))

(defn add-gross-total [evt]
  (->> (+ (:net-total evt)
          (:tax-in evt))
       util/round-currency
       (assoc* evt :gross-total)))

(defn add-invoice-no [evt]
  (->> [:customer-id :number :version]
       (map evt)
       (join ".")
       (assoc* evt :invoice-no)))

;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
;; FIXME there is no field settled anymore
(defn add-tax-period [evt]
  (->> (if-let [date (:settled evt)]
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
       (assoc* evt :tax-period)))

;; FIXME there is no field settled anymore
(defn add-ledger-state
  "Sets `:ledger-state` to either `*` or `!`, depending on the presence
  of `:settled`"
  [evt]
  (->> (if (:settlement evt) "*" "!")
       (assoc* evt :ledger-state)))

;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-period [evt]
  (->> (let [date (:date evt)]
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
       (assoc* evt :period)))

(defn add-templates [evt]
  (-> evt
      (assoc* :report-template
              (get-in @config [:invoice :report :template]))
      (assoc* :latex-template
              (get-in @config [:invoice :latex :template]))
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :invoice]))))

(defn order-items-by-amount [evt]
  (merge evt
         {:items (reverse (sort-by :amount (:items evt)))}))

(defn add-latex-content [evt]
  (->> evt
       templating/render-latex
       (assoc* evt :latex-content)))

(defn add-latex-directory [evt]
  (let [directory (get-in @config [:invoice :latex :directory])]
    (->> (templating/template directory evt)
         (assoc* evt :latex-directory))))

(defn add-latex-filename [evt]
  (let [filename (get-in @config [:invoice :latex :filename])]
    (->> (templating/template filename evt)
         (assoc* evt :latex-filename))))

;; FIXME there is no field settled anymore
(defn add-deferral [evt]
  (let [booking-year (-> evt :date .getFullYear)
        settled-date (-> evt :settled)
        settled-year (if settled-date
                       (-> settled-date .getFullYear)
                       (inc booking-year))]
    (assoc* evt :deferral (< booking-year settled-year))))

;; TODO refactor everything so that latex has its own submap wuth
;; content, path etc. so we can have a generic write! function which
;; takes a invoice and a key to the submap, see commented function
;; below.
(defn write-latex! [{directory :latex-directory
                     filename :latex-filename
                     content :latex-content
                     :as evt}]
  ;; TODO use some path join here
  (-> (str directory "/" filename)
      (util/spit content))
  evt)

;; (defn write! [event format]
;;   (->> [:path :content]
;;        (map (format event))
;;        (apply util/spit)))

(defn add-pdflatex-cmd [{directory :latex-directory
                         filename :latex-filename
                         :as evt}]
  (->> (str "(cd " directory " && pdflatex " filename ")")
       (assoc* evt :pdflatex-cmd)))

(defn run-pdflatex! [{cmd :pdflatex-cmd
                      :as evt}]
  (util/sh cmd)
  evt)

(defn transform-latex! [evt]
  (-> evt
      order-items-by-amount
      add-latex-content
      add-latex-directory
      add-latex-filename
      write-latex!
      add-pdflatex-cmd
      run-pdflatex!
      ;; TODO run xdg-open on the pdf file
      ))

(defmethod transform :invoice [context event]
  (-> event
      (common/validate! ::event)
      merge-defaults
      lookup-customer
      (resolve-settlement (:settlement context))
      common/add-iso-date
      add-deferral
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
      add-templates
      (common/validate! ::event)))
