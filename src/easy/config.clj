(ns easy.config
  (:require [easy.util :as util]))

;; TODO: `config` should probably instead be named `environment`

(def default-config
  "The easy config file is a YAML file with the following structure, e.g.
  ```
  ---
  customers: customers.yml

  templates:
    ledger:
      plain: vorlagen/plain.dat.hbs
      expense: vorlagen/expense.dat.hbs
      invoice: vorlagen/invoice.dat.hbs
      settlement: vorlagen/settlement.dat.hbs
      opening: vorlagen/opening.dat.hbs
      refund: vorlagen/refund.dat.hbs
      salary: vorlagen/salary.dat.hbs
      redistribution: vorlagen/redistribution.dat.hbs
      outlay: vorlagen/outlay.dat.hbs
      reconciliation: vorlagen/reconciliation.dat.hbs
    output:
      overview: vorlagen/overview.txt.hbs

  invoice:
    report:
      template: vorlagen/report.txt.hbs
    latex:
      template: vorlagen/invoice.tex.hbs
      directory: 'kunden/{{customer.name}}/rechnungen'
      filename: '{{iso-date}}_200ok_R-{{replace invoice-no \".\" \"_\"}}.tex'
  ```
  "
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

(defn load-config-file []
  (if (util/file-exists? ".easy.yml")
    (util/parse-yaml (slurp ".easy.yml"))
    {}))

(defn load! []
  (swap! config
         util/deep-merge
         (load-config-file)))
