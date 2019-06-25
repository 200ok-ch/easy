(ns easy.settlement
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]
            [easy.templating :as templating]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.settlement.item :as item]
            [easy.customers :as customers]
            [clojure.string :refer [join replace split]]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]))

;; spec

(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))
(def match-period (partial re-matches #"^\d{4}-(H|Q)\d$"))

;; required
(s/def ::type #{"settlement"})
(s/def ::date util/date?)
(s/def ::amount float?) ;; this should be equal to the invoice's gross-total

(s/def ::customer-id pos-int?)
(s/def ::number pos-int?) ;; sequence
(s/def ::version pos-int?)

(s/def ::invoice-no (s/and string? match-invoice-no))

(s/def ::with-invoice-no (s/or :joined (s/keys :req-un [::invoice-no])
                               :split (s/keys :req-un [::customer-id
                                                       ::number
                                                       ::version])))

(s/def ::items (s/coll-of ::item/item))

;; optional
(s/def ::iso-date (s/and string? common/match-iso-date))
(s/def ::tax-rate-in float?)
(s/def ::tax-rate-out float?)
(s/def ::tax-in float?)
(s/def ::tax-out float?)
(s/def ::tax-win float?)
(s/def ::net-total float?)
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
                                 ::amount
                                 ::items]
                        :opt-un [::customers/customer
                                 ::iso-date
                                 ::tax-rate-in
                                 ::tax-rate-out
                                 ::tax-in
                                 ::tax-out
                                 ::tax-win
                                 ::net-total
                                 ::tax-period
                                 ::ledger-state
                                 ::ledger-template
                                 ::latex-template
                                 ::latex-content
                                 ::latex-directory
                                 ::latex-filename
                                 ::pdflatex-cmd])
                ::with-invoice-no))


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


(defn add-invoice-no [evt]
  (->> [:customer-id :number :version]
       (map evt)
       (join ".")
       (assoc* evt :invoice-no)))


(defn merge* [a b]
  (reduce (fn [acc [key val]] (assoc* acc key val)) a b))


(defn spy [x] (prn x) x)


(defn add-invoice-no-details [{:keys [invoice-no] :as evt}]
  (if invoice-no
    (->> (split invoice-no #"\.")
         (map int)
         (zipmap [:customer-id :number :version])
         (merge* evt))
    evt))

(def unify-invoice-no
  (comp add-invoice-no
        add-invoice-no-details))

(defn lookup-invoice [{:keys [invoice-no] :as evt} context]
  (->> context
       (map unify-invoice-no)
       (filter #(= invoice-no (:invoice-no %)))
       first
       (assoc* evt :invoice)))


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
  (->> (:amount evt)
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
  (->> evt
       :amount
       (/ tax-rate-in)
       util/round-currency
       (assoc* evt :net-total)))



;; TODO rewrite in a way that it does not need to be adjusted for
;; every year
(defn add-tax-period [evt]
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



(defmethod transform :settlement [context event]
  (-> event
      (common/validate! ::event)
      merge-defaults
      unify-invoice-no
      lookup-customer
      (lookup-invoice context)
      common/add-iso-date
      add-tax-period
      add-tax-rate-in
      add-tax-rate-out
      transform-items
      add-net-total
      add-tax-in
      add-tax-out
      add-tax-win
      add-templates
      (common/validate! ::event)))
