(ns easy.core
  "This is the entry point. The -main function gets called from lumo.
  This namespace also provides functions for all eas subcommands."
  (:require [easy.util :as util]
            [easy.config :as config :refer [config]]
            [easy.customers :as customers]
            [easy.templating :as templating]
            [easy.transform :refer [safe-transform]]
            [easy.overview :as overview]
            [easy.invoice :as invoice]
            [easy.common :as common]
            [easy.log :as log]
            [easy.common.invoice-no :as invoice-no]
            [cljs.pprint :refer [pprint]]
            [cljs.spec.alpha :as s]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :refer [join split replace]]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.data :refer [diff]]
            ;; NOTE: Even though we don't use any of the remaining
            ;; namespaces in this list, we nevertheless have to
            ;; require them here, otherwise they won't get loaded at
            ;; all.
            easy.plain
            easy.expense
            easy.refund
            easy.opening
            easy.reconciliation
            easy.salary
            easy.outlay
            easy.settlement
            easy.redistribution
            easy.dctd))


;; (sub-)commands


(defn ledger!
  "Transforms all events, renders and prints their ledger
  representation."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map invoice-no/unify)
         ;; transform all events within the `context`
         (map (partial safe-transform context))
         flatten
         ;; filter to the events that belong to the year given with -y
         ;; TODO: do not filter when year is not given
         (filter #(.startsWith (:iso-date %) (:year options)))
         ;; make the order reproducible
         (sort-by :source-path)
         (map templating/render-ledger)
         (join "\n")
         println)))


(defn invoice!
  "Generates an invoice PDF and prints a report."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (filter #(= "invoice" (:type %)))
         (map invoice/add-invoice-no)
         (filter #(= (:invoice-no %) (:no options)))
         (util/assert-only-one! "Error: Check if you've entered the right invoice-no")
         first
         (safe-transform context)
         invoice/transform-latex!
         templating/render-report
         println)))


(defn transform!
  "Transforms all input events pretty prints the result and exits."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map (partial safe-transform context))
         flatten
         ;; This filters events to the ones where the filter matches,
         ;; as this is applied after transformation, you can filter
         ;; for anything.
         (filter #(nil? (second (diff % (:filter options)))))
         util/write-yaml
         ;; prn-str
         println)))


(defn noop!
  "Does nothing."
  []
  (do))


(defn validate!
  "Validates all input events and exits. Good to check for warnings."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map (partial safe-transform context))
         flatten
         doall)))


(defn overview!
  "Renders an overview."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map (partial safe-transform context))
         flatten
         overview/crunch-numbers
         templating/render-overview
         println)))


;; main


(defn read-stdin
  "Reads from standard in until end, then calls `callback` with the
  result as its argument."
  [callback]
  (let [stdin process.stdin
        input (atom [])
        receive-data (partial swap! input conj)
        receive-end (fn []
                      (swap! input join)
                      (callback @input))]
    (.resume stdin)
    (.setEncoding stdin "utf8")
    (.on stdin "data" receive-data)
    (.on stdin "end" receive-end)))


(defn run
  "Dispatches to a command function and exits the process afterwards."
  [command options yaml-events]
  (let [events (->> yaml-events
                    util/parse-yaml
                    (util/validate! ::common/events)
                    ;; mild transformation
                    (map common/harmonize)
                    (map invoice-no/unify)
                    (util/validate! ::common/events))]
    (case command
      :noop (noop!)
      :ledger (ledger! events options)
      :invoice (invoice! events options)
      :transform (transform! events options)
      :validate (validate! events options)
      :overview (overview! events options)
      (do ;; <- else
        (println (str "Unknown command: " command))
        (process.exit 1)))
    ;; all good, exit nicely
    (process.exit 0)))


;; FIXME: find a nice syntax to pass filters reliably via commandline
;; args
(defn- parse-filter [arg]
  ;;(println arg)
  ;;(process.exit 0)
  (read-string arg))
;;   (->> (split arg #",")
;;        (map #(split % #"="))
;;        flatten
;;        (apply hash-map)
;;        keywordize-keys))


(def cli-options
  [["-d" "--debug" "Debug output"]
   ["-i" "--input INPUT" "Input file"]
   ["-y" "--year NUMBER" "Year (only applies to the subcommand: ledger)"]
   ["-n" "--no NUMBER" "Invoice No (only applies to the subcommand: invoice)"]
   ["-f" "--filter FILTER" "Filter (only applies to the subcommand: transform)" :parse-fn parse-filter]])


;; TODO: get rid of the warning by using reader conditionals in
;;
;;   https://github.com/clojure/tools.cli/blob/master/src/main/clojure/clojure/tools/cli.cljc
;;
;; Wouldn't this be a nice open source contribution? Achieve some
;; laurels! Do it! Now!
(defn -main
  "The main function which is called by lumo. It builds the environment
  and reads the input, then dispatches to `run`."
  [& args]
  (let [cli (parse-opts args cli-options)
        command (-> cli :arguments first keyword)
        options (-> cli :options)
        ;; TODO: check early if `command` was given
        ;; TODO: build env here, doesn't need to be an atom!
        runner (partial run command options)]
    (config/load!)
    (swap! config assoc :options cli)
    (swap! config assoc :customers (customers/load))

    (if-let [path (-> cli :options :input)]
      ;; read yaml for path then call `run`
      (runner (util/slurp path))
      ;; else read input from stdin then call `run`
      (read-stdin runner))))
