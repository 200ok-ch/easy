(ns easy.collect
  (:require [lumo.util :as lumo]
            [easy.util :as util]
            [easy.log :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [cljs.pprint :refer [pprint]]
            [cljs.spec.alpha :as s]
            [clojure.string :refer [join]]))


(def cli-options
  [;; ["-d" "--debug" "Debug output"]
   ])


(defn- read-and-parse
  "Reads & parses YAML files (incl. applying any frontmatter event
  templates & source annotation)."
  [path]
  (-> path
      util/slurp
      util/parse-yaml
      (util/annotate path)))


(defn- harmonize [events]
  (map (partial util/harmonize-date-field :date) events))


(defn -main
  [& args]
  (let [cli (parse-opts args cli-options)
        path (or (-> cli :arguments first) ".")
        options (-> cli :options)]
    (->> path
         lumo/file-seq
         (filter #(re-matches #".+\.yml$" %))
         (map read-and-parse)
         (apply concat)
         harmonize
         (sort-by :date)
         util/write-yaml
         println)))
