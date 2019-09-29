(ns easy.customers
  "An example customers file looks like this:
  ```
  - name: vr
    year: 2015
    number: 3
    address: |
      Voice Republic Media AG
      Langstr. 10
      8004 Zürich
    rate: 100
    deadline: 14
    contact: info

  - name: sva
    year: 2014
    number: 2
    rate: 100
    address: |
      SVA Zürich
      Röntgenstrasse 17
      Postfach
      8087 Zürich
    contact: phil
  ```"
  (:require [cljs.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [replace join]]
            [easy.util :as util :refer [assoc*]]
            [easy.config :refer [config]]))


;; NOTE: minimum 3 maximum 10 characters is a sensible limits for
;; customers' shortnames
(def match-shortname (partial re-matches #"[a-z]{3,10}"))

;; spec - required
(s/def ::year pos-int?) ;; when they became a customer
(s/def ::number pos-int?) ;; a unique customer number
(s/def ::address string?) ;; multiline postal address
(s/def ::shortname (s/and string? match-shortname))

;; spec - optional
(s/def ::discount (s/and float? #(>= % 0) #(< % 100))) ;; a general discount in percentage
(s/def ::contact string?)
(s/def ::rate float?) ;; the default hourly rate for a given customer
(s/def ::address-for-latex string?)

(s/def ::customer (s/keys :req-un [::name
                                   ::year
                                   ::number
                                   ::address]
                          :opt-un [::discount
                                   ::contact
                                   ::rate
                                   ::address-for-latex]))

(s/def ::customers (s/coll-of ::customer))


;; defaults


(def defaults
  {:deadline 30
   :discount 0})


(def merge-defaults
  (partial merge defaults))


;; load


(defn- latex-line-breaks [s]
  (replace s "\n" "\\\\\n"))


(defn- add-address-for-latex [customer]
  (->> customer
       :address
       latex-line-breaks
       (assoc* customer :address-for-latex)))


(defn- add-year-number [customer]
  (->> (map customer [:year :number])
       (join "-")
       (assoc* customer :year-number)))


(defn- transform [customer]
  (-> customer
      add-address-for-latex
      add-year-number))


(defn load []
  (->> @config
       :customers
       ;; TODO: check if file exists, error otherwise
       util/slurp
       util/parse-yaml
       (util/validate! ::customers)
       (map merge-defaults)
       (map transform)
       (util/validate! ::customers)))
