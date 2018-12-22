(ns easy.customers
  (:require [cljs.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [replace join]]
            [easy.util :as util :refer [assoc*]]
            [easy.config :refer [config]]))

;; ------------------------------------------------------------
;; spec

;; required
(s/def ::shortname string?) ;; TODO use regex to ensure that it can be used in a fs path
(s/def ::year pos-int?) ;; when they became a customer
(s/def ::number pos-int?)
(s/def ::address string?)

;; optional
(s/def ::discount (s/and float? #(>= % 0) #(< % 100))) ;; in percentage
(s/def ::contact string?) ;; TODO make sure there is a file which is used for latex for it
(s/def ::rate float?) ;; hourly
(s/def ::address-for-latex string?) ;; where every newline is preceded by "\\"

(s/def ::customer (s/keys :req-un [::name
                                   ::year
                                   ::number
                                   ::address]
                          :opt-un [::discount
                                   ::contact
                                   ::rate
                                   ::address-for-latex]))

(s/def ::customers (s/coll-of ::customer))

;; ------------------------------------------------------------
;; defaults

(def defaults
  {:deadline 30
   :discount 0})

(def merge-defaults
  (partial merge defaults))

;; ------------------------------------------------------------
;; load

(defn- latex-line-breaks [s]
  (replace s "\n" "\\\\\n"))

(defn- add-address-for-latex [customer]
  (->> customer
       :address
       latex-line-breaks
       (assoc* customer :address-for-latex)))

(defn add-year-number [customer]
  (->> (map customer [:year :number])
       (join "-")
       (assoc* customer :year-number)))

(defn transform [customer]
  (-> customer
      add-address-for-latex
      add-year-number))

(defn load []
  (->> @config
       :customers
       util/slurp
       util/parse-yaml
       (util/validate! ::customers)
       (map transform)
       (util/validate! ::customers)))
