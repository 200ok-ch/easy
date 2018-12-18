(ns easy.core
  "This is the entry point. The -main function gets called from lumo."
  (:require ["handlebars" :as hbs] ;; via npm
            ["handlebars-helpers" :as hbsh]
            [easy.util :as util] ;; from this codebase
            [easy.config :as config]
            [easy.common :as common]
            [easy.transform :refer [transform]]
            easy.revenue
            easy.expense
            [cljs.pprint :refer [pprint]] ;; from clojurescript stdlib
            [cljs.spec.alpha :as s]
            [clojure.string :refer [join]]))

;; ------------------------------------------------------------
;; templating

(hbsh) ;; attaches the handlebars-helpers

;; TODO don't read and parse the same template over and over again
(defn- apply-template [template-key event]
  (let [path (template-key event)
        source (util/slurp path)
        renderer (hbs/compile source)]
    (renderer (clj->js event))))

(def render-ledger
  (partial apply-template :ledger-template))

(def render-latex
  (partial apply-template :latex-template))

;; ------------------------------------------------------------
;; subcommands

;; TODO dry up code duplication in the command functions

(defn ledger! [source & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)
       (map render-ledger)
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
       render-latex
       println))

(defn transform! [source & args]
  (->> source
       util/slurp
       util/parse-yaml
       (util/validate! ::common/events)
       (map transform)
       pprint))

;; ------------------------------------------------------------
;; main

(defn -main [command & args]
  (config/load!)
  (case (keyword command)
    :ledger (apply ledger! args)
    :invoice (apply invoice! args)
    :transform (apply transform! args)
    (println (str "Unknown command: " command))))
