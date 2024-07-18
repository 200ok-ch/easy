(ns easy.core
  "This is the entry point. The -main function gets called from lumo.
  This namespace also provides functions for all eas subcommands."
  (:gen-class)
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
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.data :refer [diff]]
            [clojure.pprint :as pprint]
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
            easy.dctd
            easy.pfcc))

(defn- read-and-parse
  "Reads & parses YAML files (incl. applying any frontmatter event
  templates & source annotation)."
  [path]
  (util/warn "DEBUG READ-AND-PARSE" path)
  (-> path
      util/slurp
      util/parse-yaml
      (util/annotate path)))

(defn- harmonize [events]
  (map (partial util/harmonize-date-field :date) events))

;;; special commands

(defn collect! [cli]
  (let [path (or (-> cli :arguments second) ".")]
    (->> path
         util/file-seq
         (filter #(re-matches #"[^.].*\.yml$" %))
          (map read-and-parse)
          (apply concat)
          harmonize
          (sort-by :date)
          util/write-yaml
          println)))

;;; commands over events

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
         ;; TODO: this should instead be done by each type
         (map #(update % :amount util/int-if-whole))
         (map templating/render-ledger)
         (str/join "\n")
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

;;; main

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
        (util/exit 1)))
    ;; all good, exit nicely
    (util/exit 0)))

;; FIXME: find a nice syntax to pass filters reliably via commandline
;; args
(defn- parse-filter [arg]
  ;;(println arg)
  ;;(util/exit 0)
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

(defn -main
  "The main function which is called by lumo. It builds the environment
  and reads the input, then dispatches to `run`."
  [& args]
  (let [cli (parse-opts args cli-options)
        command (-> cli :arguments first keyword)
        options (-> cli :options doall)
        ;; TODO: check early if `command` was given
        ;; TODO: build env here, doesn't need to be an atom!
        runner (partial run command options)]
    (if (#{:collect} command)
      (collect! cli)
      (do
        (config/load!)
        (swap! config assoc :options cli)
        (swap! config assoc :customers (customers/load))
        (if-let [path (-> cli :options :input)]
          ;; read yaml for path then call `run`
          (runner (slurp path))
          ;; else read input from stdin then call `run`
          (runner (slurp *in*)))))))
