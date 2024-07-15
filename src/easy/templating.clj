(ns easy.templating
  "This namespace provides the interface to all things templating."
  (:require [hbs.core :as hbs]
            [hbs.helper :as hbsh]
            [easy.util :as util]))

;; TODO: use https://github.com/leapfrogtechnology/just-handlebars-helpers because it has sprintf

;;; templating

(def registry (hbs/registry (hbs/file-loader "" "")))

(hbsh/defhelper uuid [ctx options]
  (hbsh/safe-str (str (random-uuid))))

(hbsh/register-helper! registry "uuid" uuid)

;; TODO: don't read and parse the same template over and over again
(defn- apply-template [template-key event]
  ;; (println (:type event) (:source event) (template-key event))
  (let [path (template-key event)
        ;; _ (println "hello" path)
        source (util/slurp path)]
    (hbs/render source event)))

(def render-ledger
  (partial apply-template :ledger-template))

(def render-latex
  (partial apply-template :latex-template))

(def render-report
  (partial apply-template :report-template))

(def template hbs/render)

(defn render-overview [data]
  (apply-template :overview-template data))
