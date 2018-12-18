(ns easy.core
  "This is the entry point. The -main function gets called from lumo."
  (:require
   ;; via npm
   ["handlebars" :as hbs]
   ["handlebars-helpers" :as hbsh]

   ;; from this codebase
   [easy.util :as util]
   [easy.config :as config]
   [easy.common :as common]
   [easy.transform :refer [transform]]
   easy.revenue
   easy.expense

   ;; from clojurescript stdlib
   [cljs.pprint :refer [pprint]]
   [cljs.spec.alpha :as s]
   [clojure.string :refer [join]]))

;; attach handlebars-helpers
(hbsh)

(defn apply-template [template-key event]
  (let [path (template-key event)
        source (util/slurp path)
        renderer (hbs/compile source)]
    (renderer (clj->js event))))

(def render-ledger
  (partial apply-template :ledger-template))

(def render-latex
  (partial apply-template :latex-template))

;; TODO dry up code duplication in the command functions

(defn ledger! [& args]
  ;; TODO if (first args) is a directory work on all yaml files
  (let [content (util/slurp (first args))
        events (util/parse-yaml content)]
    (util/validate! ::common/events events)
    (let [output (map transform events)
          ledger (map render-ledger output)]
      (println (join "\n" ledger)))))

(defn invoice! [source no & args]
  (let [content (util/slurp source)
        raw-events (util/parse-yaml content)]
    (util/validate! ::common/events raw-events)
    (let [events (map transform raw-events)
          event (first (filter #(= (:invoice-no %) no) events))]
      (println (render-latex event)))))

(defn transform! [& args]
  ;; TODO if (first args) is a directory work on all yaml files
  (let [content (util/slurp (first args))
        events (util/parse-yaml content)]
    (util/validate! ::common/events events)
    (let [output (map transform events)]
      (pprint output))))

(defn -main [command & args]
  (config/load!)
  (case (keyword command)
    :ledger (apply ledger! args)
    :invoice (apply invoice! args)
    :transform (apply transform! args)
    (println (str "Unknown command: " command))))
