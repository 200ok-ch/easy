(ns easy.core
  "This is the entry point. The -main function gets called from lumo."
  (:require [easy.util :as util] ;; from this codebase
            [easy.config :as config :refer [config]]
            [easy.customers :as customers]
            [easy.common :as common]
            [easy.templating :as templating]
            [easy.transform :refer [transform]]
            [easy.revenue :as revenue]
            easy.expense
            [cljs.pprint :refer [pprint]] ;; from clojurescript stdlib
            [cljs.spec.alpha :as s]
            [clojure.string :refer [join]]))

;; ------------------------------------------------------------
;; subcommands

;; TODO dry up code duplication in the command functions

(defn ledger! [source & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)
       (map templating/render-ledger)
       (join "\n")
       println))

(defn invoice! [source no & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)
       (filter #(= (:invoice-no %) no))
       first
       revenue/transform-latex!
       templating/render-report
       println))

(defn transform! [source & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)
       pprint))

(defn noop! [& args]
  (do)) ;; nothin'

(defn validate! [source & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)))

;; ------------------------------------------------------------
;; stdin

;; (def stdin process.stdin)
;; (def input (atom []))
;;
;; (.resume stdin)
;; (.setEncoding stdin "utf8")
;;
;; (defn receive-data [data]
;;   (swap! input conj data))
;;
;; (.on stdin "data" receive-data)
;;
;; (defn receive-end []
;;   ;; As long as @input is a vector, we're still reading input. When
;;   ;; reading input is finished @input is a string.
;;   (swap! input join)
;;   ;; TODO work with @input, i.e. continue to run the subcommand (maybe
;;   ;; decide with a flag if input from stdin is expected because this
;;   ;; changes the behavior quite drastically.)
;;   (println @input))
;;
;; (.on stdin "end" receive-end)

;; ------------------------------------------------------------
;; main

(defn -main [command & args]
  (config/load!)
  (swap! config assoc :customers (customers/load))
  (case (keyword command)
    :ledger (apply ledger! args)
    :invoice (apply invoice! args)
    :transform (apply transform! args)
    :noop (noop!)
    :validate (apply validate! args)
    ;; else
    (do
      (println (str "Unknown command: " command))
      (process.exit 1)))
  ;; all good
  (process.exit 0))
