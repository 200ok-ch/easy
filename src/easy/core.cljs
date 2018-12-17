(ns easy.core
  "This is the entry point. The -main function gets called from lumo."
  (:require
   ;; via npm
   ["handlebars" :as hbs]

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

(defn apply-template [template-key event]
  (let [path (template-key event)
        source (util/slurp path)
        renderer (hbs/compile source)]
    (renderer (clj->js event))))

(def render-ledger
  (partial apply-template :ledger-template))

(defn -main [& args]
  (config/load!)
  ;; TODO if (first args) is a directory work on all yaml files
  (let [content (util/slurp (first args))
        events (util/parse-yaml content)]
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
