(ns easy.core
  "This namespace currently does too much. I'm mostly working her ATM,
  and as I see groups of functions that should make up a namespace I
  move them there."
  (:require
   ;; from node stdlib
   ["fs" :as fs]

   ;; via npm
   ["js-yaml" :as yaml]
   ["handlebars" :as hbs]

   ;; via clojars/maven
   [cljs-time.core :as cljs-time]
   [cljs-time.format :as time]

   ;; from this codebase
   [easy.item :as item]
   [easy.event :as event]

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

(defn add-isodate [event]
  (->> event
       :date
       cljs-time/date-time
       (time/unparse iso-formatter)
       (assoc event :isodate)))

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

(defn add-tax-in [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-in revenue))
       (assoc revenue :tax-in)))

(defn add-tax-out [revenue]
  (->> (:net-total revenue)
       (* (:tax-rate-out revenue))
       (assoc revenue :tax-out)))

(defn add-tax-win [revenue]
  (->> (:tax-out revenue)
       (- (:tax-in revenue))
       (assoc revenue :tax-win)))

(defn add-item-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       ;; TODO calculate and subtract discount
       (assoc item :amount)))

(defn add-items-amount [revenue]
  (update revenue :items #(map add-item-amount %)))

(defn add-net-total [revenue]
  (->> revenue
       :items
       (map :amount)
       (reduce +)
       ;; TODO calculate and subtract discount
       (assoc revenue :net-total)))

(defn add-gross-total [revenue]
  (->> (+ (:net-total revenue)
          (:tax-in revenue))
       (assoc revenue :gross-total)))

(defn add-invoice-no [revenue]
  (->> [:customer :number :version]
       (map revenue)
       (join ".")
       (assoc revenue :invoice-no)))

(defn add-ledger-template [revenue]
  (assoc revenue :ledger-template "vorlagen/ledger.dat.hbs"))

(defmulti transform
  "Events will be transformed based on their type."
  (comp keyword :type))

(defmethod transform :revenue [event]
  (-> event
      add-isodate
      add-tax-rate-in
      add-tax-rate-out
      ;; TODO add-tax-period
      ;; TODO read-timesheets
      ;; TODO add-items-hours
      add-items-amount
      add-net-total
      add-tax-in
      add-tax-out
      add-tax-win
      add-gross-total
      add-invoice-no
      add-ledger-template))

(defmethod transform :expense [event]
  ;; TODO implement
  event)

(defn apply-template [template-key event]
  (let [path (template-key event)
        source (slurp path)
        renderer (hbs/compile source)]
    (renderer (clj->js event))))

(def render-ledger
  (partial apply-template :ledger-template))

(defn -main [& args]
  (let [content (slurp (first args))
        events (parse-yaml content)]
    (if (s/valid? ::event/events events)
      (let [;; TODO item/merge-defaults
            input-with-defaults (map event/merge-defaults events)
            output (map transform input-with-defaults)
            ledger (map render-ledger output)]
        ;; (println "INPUT EVENTS")
        ;; (pprint input-with-defaults)
        ;; (println "TRANSFORMED EVENTS")
        ;; (pprint output)
        ;; (println "LEDGER")
        (println (join "\n" ledger)))
      ;; if invalid explain...
      (s/explain ::event/events events))))
