(ns easy.util
  (:require
   ;; from node stdlib
   ["fs" :as fs]
   ;; via npm
   ["js-yaml" :as yaml]
   ["sprintf-js" :refer [sprintf]]
   ;; from clojars/maven
   [cljs-time.format :as time]))

(defn slurp [path]
  (.readFileSync fs path "utf8"))

(defn parse-yaml [string]
  (-> (yaml/load string)
      (js->clj :keywordize-keys true)))

(def date? (partial instance? js/Date))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      (last vs))))

(def iso-formatter
  (time/formatter "yyyy-MM-dd"))

(def round-currency
  (comp js/parseFloat (partial sprintf "%.2f")))
