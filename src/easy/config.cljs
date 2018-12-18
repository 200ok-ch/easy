(ns easy.config
  (:require [easy.util :as util]))

(def default-config
  {:customers "customers.yml"
   :templates
   {:ledger
    {:expense "templates/expense.dat.hbs"
     :revenue "templates/revenue.dat.hbs"}
    :latex
    {:invoice "templates/invoice.tex.hbs"}}})

(def config (atom default-config))

(defn load! []
  ;; TODO check if file exists
  (->> (util/slurp ".easy.yml")
       util/parse-yaml
       (swap! config util/deep-merge)))