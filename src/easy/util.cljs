(ns easy.util
  (:require [cljs.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [join replace]]
            ["fs" :as fs]
            ["js-yaml" :as yaml]
            ["sync-exec" :as exec]
            ["sprintf-js" :refer [sprintf]]
            [cljs-time.format :as time]
            [clojure.string :as str]))


(defn bin-by
  "Takes a fn `f` and a colection `coll`, returns a map with the value
  of `f` as keys and a list of items of `coll` for which the value of
  `f` matches the key. This is confusing, I know."
  {:test #(do
            (assert (= (bin-by :type [{:type :a :a 1} {:type :b :b 2} {:type :a :b 42}])
                       {:a ({:type :a, :b 42} {:type :a, :a 1}), :b ({:type :b, :b 2})})))}
  [f coll]
  (reduce #(update %1 (f %2) conj %2) {} coll))


(defn sanitize-latex [text]
  (when text
    (-> text
        (replace "_" " ") ; FIXME: this is a hack
        (replace "#" "\\#")
        (replace "&" "\\&"))))

(defn warn [msg]
  (.error js/console (clj->js msg))
  msg)


(defn sh [& args]
  (exec (join " " args)))


(defn spy [& args]
  (apply pprint args)
  (last args))


(defn file-exists? [path]
  (try
    (.isFile (.statSync fs path))
    (catch :default e
      ;; TODO: output an error message here?
      false)))


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


(defn exit [c]
  (.exit js/process (or c 0)))


(defn die [s]
  (warn s)
  (exit 1))


(defn indent
  "Indents a multiline string `s` by `n` spaces."
  [s n]
  (let [i (apply str (repeat n " "))]
    (str i (str/replace s "\n" (str "\n" i)))))


(defn write-yaml [x]
  (-> x
      clj->js
      yaml/safeDump
      (indent 2)))


(defn doc-reducer-dispatcher [doc]
  (cond
    (sequential? doc) :sequential
    (associative? doc) :associative
    :else (type doc)))


(defmulti doc-reducer (fn [_ doc] (doc-reducer-dispatcher doc)))


(defmethod doc-reducer :sequential [{:keys [template] :as aggregator} events]
  (update aggregator :events concat (map (partial merge template) events)))


(defmethod doc-reducer :associative [{:keys [template] :as aggregator} new-template]
  (update aggregator :template merge new-template))


(defn apply-templates [docs]
  (:events (reduce doc-reducer {:template {} :events []} docs)))


(defn parse-yaml
  "Parses YAML with mulitple docs, and joins these does by applying YAML
  event templates (associatives) to YAML event lists (sequentials) and
  concatenating these lists."
  [string]
  (-> string
      yaml/loadAll
      (js->clj :keywordize-keys true)
      apply-templates))


(defn annotate
  "Annotates all events with `:source-path '<path>:e<index>'`, where
  index is the position of the event in the source file."
  [events path]
  (map-indexed #(assoc %2 :source-path (str path ":e" %1)) events))


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


(defn include? [item collection]
  (some #{item} collection))


(defn assoc*
  "Like `assoc` but adds `key` only if hashmap does not already have
  `key` (preset). Warns if preset and `value` differ. (Also takes only
  one `key` and `value`.)"
  [hashmap key value]
  (if-let [preset (key hashmap)]
    (let [ignored (get hashmap :ignore-warnings [])]
      (if (and (not= preset value)
               (not (include? (name key) ignored)))
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
      (println (write-yaml x))
      (s/explain spec x)
      (process.exit 1))
    x))


(defn harmonize-date-field [field evt]
  (if-let [date (field evt)]
    (if (string? date)
      ;; NOTE don't use this, this does not return an instance of Date
      ;; (assoc evt field (time/parse util/iso-formatter date))
      (assoc evt field (js/Date. date))
      evt)
    evt))


(defn assert-only-one! [msg x]
  (assert (= 1 (count x)) msg)
  x)
