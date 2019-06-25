;; TODO `config` should probably instead be named `state` or `data`
(ns easy.config
  (:require [easy.util :as util]))


(def default-config
  {:customers "customers.yml"
   :templates
   {:ledger
    {:expense "templates/expense.dat.hbs"
     :revenue "templates/revenue.dat.hbs"}}
   :invoice
   {:latex
    {:template "templates/invoice.tex.hbs"
     :directory "customers/{{name}}/invoices"
     :filename "{{iso-date}}.tex"}
    :report
    {:template "templates/report.txt.hbs"}}})


(def config (atom default-config))


(defn load! []
  ;; TODO check if file exists
  (->> (util/slurp ".easy.yml")
       util/parse-yaml
       (swap! config util/deep-merge)))
