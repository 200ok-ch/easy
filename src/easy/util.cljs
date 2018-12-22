(ns easy.util
  (:require [cljs.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            ["fs" :as fs]
            ["js-yaml" :as yaml]
            ["sprintf-js" :refer [sprintf]]
            [cljs-time.format :as time]))

(defn spy [x]
  (pprint x)
  x)

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

(defn assoc*
  "Like `assoc` but adds `key` only if hashmap does not already have
  `key`. Also takes only one `key` and `value`."
  [hashmap key value]
  (if (contains? hashmap key)
    hashmap
    (assoc hashmap key value)))

(defn validate!
  "Validates `x` against `spec` and exits the process in case `x` does
  not validate. If it validates it returns `x`. (`common/validate!`
  uses this, but has the arguments swapped.)"
  [spec x]
  (if-not (s/valid? spec x)
    (do
      (s/explain spec x)
      (process.exit 1))
    x))
