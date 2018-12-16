(ns easy.core
  "This namespace currently does too much. I'm mostly working here ATM,
  and as I see groups of functions that should make up a namespace I
  move them there."
  (:require
   ;; from node stdlib
   ["fs" :as fs]

   ;; via npm
   ["js-yaml" :as yaml]
   ["handlebars" :as hbs]
   ["sprintf-js" :refer [sprintf]]

   ;; via clojars/maven
   [cljs-time.core :as cljs-time]
   [cljs-time.format :as time]

   ;; from this codebase
   [easy.common :as common]
   [easy.expense :as expense]
   [easy.revenue :as revenue]

   ;; from clojurescript stdlib
   [cljs.pprint :refer [pprint]]
   [cljs.spec.alpha :as s]
   [clojure.string :refer [join]]))

(defn slurp [path]
  (.readFileSync fs path "utf8"))

(defn parse-yaml [string]
  (-> (yaml/load string)
      (js->clj :keywordize-keys true)))

(def iso-formatter
  (time/formatter "yyyy-MM-dd"))

(defn add-iso-date [event]
  (->> event
       :date
       cljs-time/date-time
       (time/unparse iso-formatter)
       (assoc event :iso-date)))

(defn add-iso-settled [event]
  (if-let [settled (:settled event)]
    (->> settled
         cljs-time/date-time
         (time/unparse iso-formatter)
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
      ;; because we switched to effective
      (> date (time/parse "2018-12-31")) 0.77
      ;; saldo from 2018
      :else 0.065)))

(defn add-tax-rate-in [revenue]
  (->> (tax-rate-in revenue)
       (assoc revenue :tax-rate-in)))

(defn add-tax-rate-out [revenue]
  (->> (tax-rate-out revenue)
       (assoc revenue :tax-rate-out)))

(def round-currency
  (comp js/parseFloat (partial sprintf "%.2f")))

(defn add-tax-in [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-in revenue))
       round-currency
       (assoc revenue :tax-in)))

(defn add-tax-out [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-out revenue))
       round-currency
       (assoc revenue :tax-out)))

(defn add-tax-win [revenue]
  (->> (:tax-out revenue)
       (- (:tax-in revenue))
       round-currency
       (assoc revenue :tax-win)))

(defn add-item-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       ;; TODO calculate and subtract discount
       round-currency
       (assoc item :amount)))

(defn add-items-amount [revenue]
  (update revenue :items #(map add-item-amount %)))

(defn add-net-total [revenue]
  (->> revenue
       :items
       (map :amount)
       (reduce +)
       ;; TODO calculate and subtract discount
       round-currency
       (assoc revenue :net-total)))

(defn add-gross-total [revenue]
  (->> (+ (:net-total revenue)
          (:tax-in revenue))
       round-currency
       (assoc revenue :gross-total)))

(defn add-invoice-no [revenue]
  (->> [:customer :number :version]
       (map revenue)
       (join ".")
       (assoc revenue :invoice-no)))

(defn add-ledger-template [revenue path]
  (assoc revenue :ledger-template path))

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

(defmulti transform
  "Events will be transformed based on their type."
  (comp keyword :type))

(defmethod transform :revenue [event]
  (if (s/valid? ::revenue/event event)
    (-> event
        revenue/merge-defaults
        ;; TODO item/merge-defaults
        add-iso-date
        add-iso-settled
        add-ledger-state
        add-tax-rate-in
        add-tax-rate-out
        add-tax-period
        ;; TODO read-timesheets
        ;; TODO add-items-hours
        add-items-amount
        add-net-total
        add-tax-in
        add-tax-out
        add-tax-win
        add-gross-total
        add-invoice-no
        (add-ledger-template "vorlagen/revenue.dat.hbs"))
    ;; else explain
    (s/explain ::revenue/event event)))

(defmethod transform :expense [event]
  (if (s/valid? ::expense/event event)
    (-> event
        add-iso-date
        ;; TODO implement
        (assoc :ledger-state "*")
        (add-ledger-template "vorlagen/expense.dat.hbs"))
    ;; else explain
    (s/explain ::expense/event event)))

(defn apply-template [template-key event]
  (let [path (template-key event)
        source (slurp path)
        renderer (hbs/compile source)]
    (renderer (clj->js event))))

(def render-ledger
  (partial apply-template :ledger-template))

(defn -main [& args]
  ;; TODO if (first args) is a directory work on all yaml files
  (let [content (slurp (first args))
        events (parse-yaml content)]
    (if (s/valid? ::common/events events)
      (let [output (map transform events)
            ledger (map render-ledger output)]
        ;; (println "INPUT EVENTS")
        ;; (pprint input-with-defaults)
        ;; (println "TRANSFORMED EVENTS")
        ;; (pprint output)
        ;; (println "LEDGER")
        (println (join "\n" ledger)))
      ;; if invalid explain...
      (s/explain ::common/events events))))
