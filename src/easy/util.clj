(ns easy.util
  (:refer-clojure :exclude [slurp spit file-seq])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clj-time.format :as format]
            [clj-time.coerce :as coerce]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [clojure.data.csv :as csv])
  (:import java.util.Date
           org.joda.time.DateTime))

(defn- int-if-whole [x]
  (when x
    (if (-> x int float #{x})
      (int x)
      x)))

(defn parse-float [s]
  (Float/parseFloat s))

(defn parse-int [s]
  (Integer/parseInt s))

(defn read-csv [path]
  (with-open [reader (io/reader path)]
    (doall
     (csv/read-csv reader))))

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
        (str/replace "_" " ") ; FIXME: this is a hack
        (str/replace "#" "\\#")
        (str/replace "&" "\\&"))))

(defn warn [& args]
  (binding [*out* *err*]
    (apply println args))
  args)

(defn sh [& args]
  (apply shell/sh args))

(defn spy [& args]
  (apply pprint args)
  (last args))

(defn file-exists? [path]
  (.exists (io/file path)))

(def slurp
  clojure.core/slurp)

(def spit
  clojure.core/spit)

(def file-seq
  (comp (partial map (memfn getPath))
        clojure.core/file-seq
        io/file))

(defn exit [x]
  (System/exit x))

(defn die [s]
  (warn s)
  (exit 1))

(defn indent
  "Indents a multiline string `s` by `n` spaces."
  [s n]
  (let [i (apply str (repeat n " "))]
    (str i (str/replace s "\n" (str "\n" i)))))

(def write-yaml
  yaml/generate-string)

(defn doc-reducer-dispatcher [doc]
  (cond
    (sequential? doc) :sequential
    (map? doc) :map
    :else (type doc)))

(defmulti doc-reducer (fn [_ doc] (doc-reducer-dispatcher doc)))

(defmethod doc-reducer :default [agg doc]
  (warn (str "Don't know how to handle YAML doc of type '" (type doc) "' Skipping."))
  agg)

(defmethod doc-reducer :sequential [{:keys [template] :as aggregator} events]
  (warn "DEBUG" "SEQ")
  (update aggregator :events concat (map (partial merge template) events)))

(defmethod doc-reducer :map [{:keys [template] :as aggregator} new-template]
  (warn "DEBUG" "MAP")
  (assoc aggregator :template new-template))

(defn apply-templates [docs]
  ;; Handle one special case, when there is only one doc then return
  ;; it, this allows to read YAML documents that don't hold events to
  ;; be read by the same means
  ;; (println "-------------------------------------------------------------------------------- " (count docs) (type docs) (-> docs first type))
  (if (= 1 (count docs))
    (first docs)
    (:events (reduce doc-reducer {:template {} :events []} docs))))

(defn parse-yaml-date [date-str]
  (warn "DEBUG parsing date" date-str)
  (format/parse (format/formatter "yyyy-MM-dd") date-str))

(def ^:private custom-yaml-tags
  ;; FIXME: this doesn't seem to do anything, the dates are still of
  ;; type java.util.Date
  {java.util.Date parse-yaml-date})

(defn- convert-dates [data]
  (walk/postwalk
   (fn [x]
     (if (instance? java.util.Date x)
       (coerce/from-long (.getTime x))
       x))
   data))

(defn parse-yaml
  "Parses YAML with mulitple docs, and joins these does by applying YAML
  event templates (associatives) to YAML event lists (sequentials) and
  concatenating these lists."
  [string & {:as opts}]
  (->> (yaml/parse-string string (merge {:load-all true} opts))
       apply-templates
       convert-dates))

(defn annotate
  "Annotates all events with `:source-path '<path>:e<index>'`, where
  index is the position of the event in the source file."
  [events path]
  ;; (pprint (type (doall events)))
  (map-indexed #(assoc %2 :source-path (str path ":e" %1)) events))

(defn date? [x]
  ;; TODO
  true)

(defn deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? map? args)
                        (apply deep-merge args)
                        (last args)))
         maps))

;; (defn deep-merge [v & vs]
;;   (letfn [(rec-merge [v1 v2]
;;             (if (and (map? v1) (map? v2))
;;               (merge-with deep-merge v1 v2)
;;               v2))]
;;     (if (some identity vs)
;;       (reduce #(rec-merge %1 %2) v vs)
;;       (last vs))))

(def iso-formatter
  (format/formatter "yyyy-MM-dd"))

(def round-currency
  (comp edn/read-string (partial format "%.2f") float))

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
      (println (s/explain spec x))
      (exit 1))
    x))

(defn harmonize-date-field [field evt]
  ;; (println field (type (field evt)))
  (if-let [date (field evt)]
    (if (string? date)
      (assoc evt field (parse-yaml-date date))
      evt)
    evt))

(defn assert-only-one! [msg x]
  (assert (= 1 (count x)) msg)
  x)

(extend-protocol yaml/YAMLCodec
  org.joda.time.DateTime
  (encode [data]
    (format/unparse iso-formatter data)))

(def format clojure.core/format)
