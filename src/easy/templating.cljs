(ns easy.templating
  "This namespace provides the interface to all things templating."
  (:require ["handlebars" :as hbs] ;; via npm
            ["handlebars-helpers" :as hbsh]
            [easy.util :as util]))

;; TODO: use https://github.com/leapfrogtechnology/just-handlebars-helpers because it has sprintf


;; templating

(hbsh) ;; attaches the handlebars-helpers

(.registerHelper hbs "uuid", #(random-uuid))


;; TODO: don't read and parse the same template over and over again
(defn- apply-template [template-key event]
  ;; (println (:type event) " " (:source event))
  (let [path (template-key event)
        ;; _ (println "hello" path)
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


(defn render-overview [data]
  (apply-template :overview-template data))
