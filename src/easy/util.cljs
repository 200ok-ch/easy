(ns easy.util
  (:require [cljs.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [join replace]]
            ["fs" :as fs]
            ["js-yaml" :as yaml]
            ["sync-exec" :as exec]
            ["sprintf-js" :refer [sprintf]]
            [cljs-time.format :as time]))


(defn sanitize-latex [text]
  (-> text
      (replace "_" " ") ;; FIXME this is a hack
      (replace "#" "\\#")
      (replace "&" "\\&")))


(defn warn [msg]
  (.error js/console msg))


(defn sh [& args]
  (exec (join " " args)))


(defn spy [& args]
  (apply pprint args)
  (last args))


(defn slurp [path]
  {:pre [(string? path)]}
  (try
    (.readFileSync fs path "utf8")
    (catch :default e
      (println "ERROR: Cannot read file " path " due to exception " e))))


(defn spit [path content]
  {:pre [(string? path)
         (string? content)]}
  (.writeFileSync fs path content))


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
  `key` (preset). Warns if preset and `value` differ. (Also takes only
  one `key` and `value`.)"
  [hashmap key value]
  (if-let [preset (key hashmap)]
    (do
      (if (not= preset value)
        (warn (str "Overwriting " key " " value " with diffing preset " preset)))
      hashmap)
    (assoc hashmap key value)))


(defn merge*
  "A rather perculiar implementation of reverse-merge."
  [a b]
  (reduce (fn [acc [key val]] (assoc* acc key val)) a b))


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
