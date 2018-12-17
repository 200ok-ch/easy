(ns easy.config
  (:require [easy.util :as util]))

(def default-config
  {:templates
   {:expense "templates/expense.dat.hbs"
    :revenue "templates/revenue.dat.hbs"}})

(def config (atom default-config))

(defn load! []
  ;; TODO check if file exists
  (->> (util/slurp ".easy.yml")
       util/parse-yaml
       (swap! config util/deep-merge)))
