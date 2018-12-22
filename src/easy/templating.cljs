(ns easy.templating
  (:require ["handlebars" :as hbs] ;; via npm
            ["handlebars-helpers" :as hbsh]
            [easy.util :as util]))

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

(def render-report
  (partial apply-template :report-template))

(defn template [source values]
  ((hbs/compile source) (clj->js values)))
