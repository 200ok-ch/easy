(ns easy.invoice
  "An *invoice* event looks like this:
  ```
  - type: invoice
    date: 2018-04-08
    description: Acme Development
    settled: 2018-07-11
    customer-id: 3
    number: 5
    version: 1
    items:
    - rate: 220.0
      hours: 12.5
      beneficiary: employee1
    - rate: 220.0
      hours: 19.25
      beneficiary: employee2
  ```"
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.log :as log]
            [easy.common :as common]
            [easy.common.invoice-no :as invoice-no]
            [easy.common.tax :as tax]
            [easy.templating :as templating]
            [easy.config :refer [config]]
            [easy.transform :refer [transform safe-transform]]
            [easy.invoice.item :as item]
            [easy.customers :as customers]
            [clojure.string :as str]
            [clj-time.core :as time]))

;;; spec

(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))
(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))

(s/def ::type #{"invoice"})
(s/def ::date util/date?)
(s/def ::items (s/coll-of ::item/item))

(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::deadline pos-int?) ;; in days
(s/def ::header string?)
(s/def ::footer string?)
(s/def ::tax-rate-in number?)
(s/def ::tax-rate-out number?)
(s/def ::tax-in number?)
(s/def ::tax-out number?)
(s/def ::tax-win number?)
(s/def ::net-total number?)
(s/def ::gross-total number?)
(s/def ::period (s/and string? match-period))
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
                                 ::period
                                 ::ledger-state
                                 ::ledger-template
                                 ::latex-template
                                 ::latex-content
                                 ::latex-directory
                                 ::latex-filename
                                 ::pdflatex-cmd])
                ::invoice-no/with))

;;; defaults

(def defaults
  {:discount 0
   :deadline 30})

(def merge-defaults
  (partial merge defaults))

;;; transformers

;; TODO: add doc strings to all functions
;; TODO: add pre conditions to all functions

(defn- lookup-customer [{id :customer-id :as evt}]
  (->> @config
       :customers
       (filter #(= id (:number %)))
       first
       (assoc* evt :customer)))

(defn- resolve-settlement [{:keys [invoice-no] :as evt} ctx]
  (if (nil? ctx)
    evt
    (->> ctx
         (filter #(= invoice-no (:invoice-no %)))
         first
         (safe-transform nil)
         (assoc* evt :settlement))))

(defn add-tax-rate-in [evt]
  (if (:tax-free evt)
    (assoc* evt :tax-rate-in 0)
    (->> (tax/lookup-rate :vat-tax-rate-in evt)
         (assoc* evt :tax-rate-in))))

(defn add-tax-rate-out [evt]
  (if (:tax-free evt)
    (assoc* evt :tax-rate-out 0)
    (->> (tax/lookup-rate :vat-tax-rate-out evt)
         (assoc* evt :tax-rate-out))))

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
  (update evt :items (partial map #(item/transform % evt))))

(defn add-net-total-before-discount [evt]
  (->> evt
       :items
       (map :amount)
       (reduce +)
       util/round-currency
       (assoc* evt :net-total-before-discount)))

(defn add-discount-amount [evt]
  (->> evt
       :net-total-before-discount
       (* (/ (evt :discount) 100))
       util/round-currency
       (assoc* evt :discount-amount)))

(defn add-net-total [evt]
  (->> evt
       :discount-amount
       (- (evt :net-total-before-discount))
       ;; util/round-currency
       (assoc* evt :net-total)))

(defn add-gross-total [evt]
  (->> (+ (:net-total evt)
          (:tax-in evt))
       util/round-currency
       (assoc* evt :gross-total)))

(defn add-invoice-no [evt]
  (->> [:customer-id :number :version]
       (map evt)
       (str/join ".")
       (assoc* evt :invoice-no)))

;; FIXME there is no field settled anymore
(defn add-ledger-state
  "Sets `:ledger-state` to either `*` or `!`, depending on the presence
  of `:settled`"
  [evt]
  (->> (if (:settlement evt) "*" "!")
       (assoc* evt :ledger-state)))

(defn add-templates [evt]
  (-> evt
      (assoc* :report-template
              (get-in @config [:invoice :report :template]))
      (assoc* :latex-template
              (get-in @config [:invoice :latex :template]))
      (assoc* :ledger-template
              (get-in @config [:templates :ledger :invoice]))))

(defn add-texinputs-directory [evt]
  (->> (get-in @config [:texinputs-directory])
       (assoc* evt :texinputs-directory)))

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

;; this can only be inferred when settlement has been resolved
(defn add-deferral [evt]
  (if-let [settlement (-> evt :settlement)]
    (assoc* evt :deferral (not= (-> evt :date time/year)
                                (-> settlement :date time/year)))
    (assoc* evt :deferral true)))

(defn add-deadline-iso-date [evt]
  (as-> evt %
    (:date %)
    (time/plus % (time/days (:deadline evt)))
    (common/make-iso-date %)
    (assoc* evt :deadline-iso-date %)))

;; TODO: refactor everything so that latex has its own submap wuth
;; content, path etc. so we can have a generic write! function which
;; takes a invoice and a key to the submap, see commented function
;; below.
(defn write-latex! [{directory :latex-directory
                     filename :latex-filename
                     content :latex-content
                     :as evt}]
  ;; TODO: use some path join here
  (-> (str directory "/" filename)
      (util/spit content))
  evt)

;; (defn write! [event format]
;;   (->> [:path :content]
;;        (map (format event))
;;        (apply util/spit)))

(defn add-pdflatex-cmd
  [{:keys [latex-directory
           latex-filename
           texinputs-directory]
    :as evt}]
  (->> (str "(cd " latex-directory
            " && TEXINPUTS=.:" texinputs-directory ":"
            " pdflatex " latex-filename ")")
       (assoc* evt :pdflatex-cmd)))

(defn run-pdflatex! [{cmd :pdflatex-cmd
                      :as evt}]
  (util/sh cmd)
  evt)

;; TODO: why use a different transform here, this should move into the
;; main transform, except for write-latex! and run-pdflatex!
(defn transform-latex! [evt]
  (-> evt
      ;; order-items-by-amount ;; CHECK: when was this a good idea?
      add-latex-content
      add-latex-directory
      add-latex-filename
      write-latex!
      add-texinputs-directory
      add-pdflatex-cmd
      run-pdflatex!
      ;; TODO: run xdg-open on the pdf file
      ))

(defmethod transform :invoice [context evt]
  (-> evt
      (common/validate! ::event)
      merge-defaults
      lookup-customer
      (resolve-settlement (:settlement context))
      common/add-iso-date
      add-deferral
      tax/add-period
      add-ledger-state
      add-tax-rate-in
      add-tax-rate-out
      transform-items
      add-net-total-before-discount
      add-discount-amount
      add-net-total
      add-tax-in
      add-gross-total
      add-tax-out
      add-tax-win
      add-templates
      add-deadline-iso-date
      (common/validate! ::event)))
