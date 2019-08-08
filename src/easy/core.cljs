(ns easy.core
  "This is the entry point. The -main function gets called from lumo."
  (:require [easy.util :as util]
            [easy.config :as config :refer [config]]
            [easy.customers :as customers]
            [easy.templating :as templating]
            [easy.transform :refer [transform]]
            [easy.overview :as overview]
            [easy.invoice :as invoice]
            easy.expense
            easy.refund
            easy.opening
            easy.reconciliation
            easy.salary
            easy.outlay
            easy.settlement
            [easy.common :as common]
            [easy.common.invoice-no :as invoice-no]
            [clojure.tools.cli :refer [parse-opts]]
            [cljs.pprint :refer [pprint]]
            [cljs.spec.alpha :as s]
            [clojure.string :refer [join]]))


;; commands


(defn ledger!
  "Transforms all events, renders and prints their ledger
  representation."
  [events options]
  ;; TODO use this in every other subcommand as well
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map invoice-no/unify)
         ;; transform all events within the `context`
         (map (partial transform context))
         ;; filter to the events that belong to the year given with -y
         ;; TODO do not filter when year is not given
         (filter #(.startsWith (:iso-date %) (:year options)))
         (map templating/render-ledger)
         (join "\n")
         println)))


(defn invoice!
  "Generates an invoice PDF and prints a report."
  [events options]
  (->> events
       (filter #(= "invoice" (:type %)))
       (map invoice/add-invoice-no)
       (filter #(= (:invoice-no %) (:no options)))
       first
       (transform events)
       invoice/transform-latex!
       templating/render-report
       println))


(defn transform!
  "Transforms all input events pretty prints the result and exits."
  [events options]
  (let [context (util/bin-by (comp keyword :type) events)]
    (->> events
         (map (partial transform context))
         pprint)))


(defn noop!
  "Does nothing."
  []
  (do))


(defn validate!
  "Validates all input events and exits."
  [events options]
  (->> events
       (map (partial transform events))))


(defn overview!
  "Renders an overview."
  [events options]
  (->> events
       (map (partial transform events))
       overview/crunch-numbers
       templating/render-overview
       println))


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
      ;; else
      (do
        (println (str "Unknown command: " command))
        (process.exit 1)))
    ;; all good, exit nicely
    (process.exit 0)))


(def cli-options
  [["-i" "--input INPUT" "Input file"]
   ["-y" "--year NUMBER" "Year"]
   ["-n" "--no NUMBER" "Invoice No"]])


;; TODO get rid of the warning by using reader conditionals in
;; https://github.com/clojure/tools.cli/blob/master/src/main/clojure/clojure/tools/cli.cljc
(defn -main
  "The main function which is called by lumo. It builds the environment
  and reads the input, then dispatches to `run`."
  [& args]
  (let [cli (parse-opts args cli-options)
        command (-> cli :arguments first keyword)
        options (-> cli :options)
        ;; TODO build env here
        runner (partial run command options)]

    ;; TODO check if `command` was given

    ;; TODO build environment, this doesn't need to be an atom
    (config/load!)
    (swap! config assoc :options cli)
    (swap! config assoc :customers (customers/load))

    (if-let [path (-> cli :options :input)]
      ;; read yaml for path then call `run`
      (runner (util/slurp path))
      ;; read input from stdin then call `run`
      (read-stdin runner))))
