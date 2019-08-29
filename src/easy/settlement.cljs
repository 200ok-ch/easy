(ns easy.settlement
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.common.invoice-no :as invoice-no]
            [easy.templating :as templating]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.settlement.item :as item]
            [easy.customers :as customers]
            [clojure.string :refer [join replace split]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; spec

(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))

;; required
(s/def ::type #{"settlement"})
(s/def ::date util/date?)
(s/def ::amount float?) ;; this should be equal to the invoice's gross-total

(s/def ::items (s/coll-of ::item/item))

;; optional
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::tax-rate-in float?)
(s/def ::tax-rate-out float?)
(s/def ::tax-in float?)
(s/def ::tax-out float?)
(s/def ::tax-win float?)
(s/def ::net-total float?)
(s/def ::remaining float?)
(s/def ::tax-period (s/and string? match-period))
(s/def ::ledger-template (s/and string? common/match-template))
(s/def ::latex-template (s/and string? common/match-template))
(s/def ::latex-content string?)
(s/def ::latex-directory string?)
(s/def ::latex-filename string?)
(s/def ::pdflatex-cmd string?)

(s/def ::event (s/and
                (s/keys :req-un [::type
                                 ::date
                                 ::amount]
                        :opt-un [::customers/customer
                                 ::iso-date
                                 ::tax-rate-in
                                 ::tax-rate-out
                                 ::tax-in
                                 ::tax-out
                                 ::tax-win
                                 ::net-total
                                 ::remaining
                                 ::tax-period
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
  {})


(def merge-defaults
  (partial merge defaults))


;; transformer

;; TODO add doc strings to all functions
;; TODO add pre conditions to all functions


(defn lookup-customer [{id :customer-id :as evt}]
  (->> @config
       :customers
       (filter #(= id (:number %)))
       first
       (assoc* evt :customer)))


(defn resolve-invoice [{:keys [invoice-no] :as evt} context]
  (if (nil? context)
    evt
    (->> context
         (filter #(= invoice-no (:invoice-no %)))
         first
         (transform nil)
         (assoc* evt :invoice))))


(defn assert-invoice! [{:keys [invoice invoice-no] :as evt} context]
  (if (and context (not invoice))
    (do
      (util/warn (str "No invoice for settlement '" invoice-no "'. Abort."))
      (util/exit 1))
    evt))


;; this can only be infered when invoice has been resolved
(defn add-deferral [evt]
  (if-let [invoice (-> evt :invoice)]
    (assoc* evt :deferral (not= (-> evt :date .getFullYear)
                                (-> invoice :date .getFullYear)))
    evt))


;; TODO make the tax rate configurable via config
(defn tax-rate-in [evt]
  (if (< (-> evt :invoice :date)
         (time/parse "2018-01-01"))
    0.08
    0.077))

;; TODO make the tax rate configurable via config
(defn tax-rate-out [evt]
  (let [date (-> evt :invoice :date)]
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
  (->> (:net-total evt)
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


(defn add-net-total
  "Calculates the net-total based on the gross-total (here `amount`)."
  [evt]
  (->> (/ (:amount evt) (inc (:tax-rate-in evt)))
       util/round-currency
       (assoc* evt :net-total)))



;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-tax-period
  "The tax-period is when the vat is due."
  [evt]
  (->> (let [date (-> evt :date)]
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
       (assoc* evt :tax-period)))

(defn add-templates [evt]
  (-> evt
      (assoc* :report-template
              (get-in @config [:invoice :report :template]))
      (assoc* :latex-template
              (get-in @config [:invoice :latex :template]))
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :settlement]))))

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


;; TODO refactor everything so that latex has its own submap with
;; content, path etc. so we can have a generic write! function which
;; takes a settlement (evt) and a key to the submap, see commented
;; function below.
(defn write-latex! [{directory :latex-directory
                     filename :latex-filename
                     content :latex-content
                     :as evt}]
  ;; TODO use some path join here
  (-> (str directory "/" filename)
      (util/spit content))
  evt)

;; (defn write! [evt format]
;;   (->> [:path :content]
;;        (map (format evt))
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


;; this can only be calculated if invoice is already resolved
(defn add-coverage [evt]
  (if (:invoice evt)
    (let [settlement-total (:net-total evt)
          invoice-total (->> evt :invoice :net-total)
          coverage (/ settlement-total invoice-total)]
      (if (and (not (common/ignore-warning? evt :coverage))
               (or (< coverage 0.98)
                   (> coverage 1.02))
        (util/warn (str "Coverage " coverage " on settlement for " (:invoice-no evt) " " (-> evt :invoice :description))))
      (assoc* evt :coverage coverage))
    evt))


(defn add-distribution [evt]
  (let [total (-> evt :invoice :amount)]
    (->> evt
         :invoice
         :items
         (map (fn [i] (update i :amount #(util/round-currency (* % (:coverage evt))))))
         vec ;; for iterating in handlebars templates this needs to be a vector, not a list!
         (assoc* evt :distribution))))


(defn add-remaining [evt]
  (if-let [invoice (-> evt :invoice)]
    (let [invoice-amount (-> invoice :gross-total)
          payment-amount (-> evt :amount)
          remaining (util/round-currency (- invoice-amount payment-amount))]
      (if (not= 0 remaining)
        (assoc* evt :remaining remaining)
        evt))
    evt))


(defn add-debug [evt]
  (assoc* evt :debug (prn-str evt)))

;; `context` can be a map of types and vectors of events
;;
;; `context` can also be nil, this is the case if the event is
;; transformed while being resolved for another event
(defmethod transform :settlement [context event]
  (-> event
      (common/validate! ::event)
      merge-defaults
      lookup-customer
      (resolve-invoice (:invoice context))
      (assert-invoice! context)
      add-deferral
      common/add-iso-date
      add-tax-period
      add-tax-rate-in
      add-tax-rate-out
      transform-items
      add-net-total
      add-coverage
      add-remaining
      add-distribution
      add-tax-in
      add-tax-out
      add-tax-win
      add-templates
      add-debug
      (common/validate! ::event)))
