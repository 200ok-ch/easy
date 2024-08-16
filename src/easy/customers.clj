(ns easy.customers
  "An example customers file looks like this:
  ```
  - name: acme
    year: 2015
    number: 3
    address: |
      Acme Corp
      Langstr. 1
      8004 Zürich
    rate: 100
    deadline: 14
    contact: info

  - name: wile
    year: 2014
    number: 2
    rate: 100
    address: |
      Wile E. Coyote
      Langstr. 2
      8004 Zürich
    contact: staff
  ```"
  (:refer-clojure :exclude [load])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [easy.util :as util :refer [assoc*]]
            [easy.config :refer [config]]))

;;; spec

;; NOTE: minimum 3 maximum 10 characters is a sensible limits for
;; customers' shortnames
(def match-shortname (partial re-matches #"[a-z]{3,10}"))

(s/def ::year pos-int?) ;; when they became a customer
(s/def ::number pos-int?) ;; a unique customer number
(s/def ::address string?) ;; multiline postal address
(s/def ::shortname (s/and string? match-shortname))

(s/def ::discount (s/and number? #(>= % 0) #(< % 100))) ;; a general discount in percentage
(s/def ::contact string?)
(s/def ::rate number?) ;; the default hourly rate for a given customer
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

;;; defaults

(def defaults
  {:deadline 30
   :discount 0})

(def merge-defaults
  (partial merge defaults))

;;; load

(defn- latex-line-breaks [s]
  (str/replace s "\n" "\\\\\n"))

(defn- add-address-for-latex [customer]
  (->> customer
       :address
       latex-line-breaks
       (assoc* customer :address-for-latex)))

(defn- add-year-number [customer]
  (->> (map customer [:year :number])
       (str/join "-")
       (assoc* customer :year-number)))

(defn- transform [customer]
  (-> customer
      add-address-for-latex
      add-year-number))

(defn load []
  (let [path (:customers @config)]
    (if (util/file-exists? path)
      (->> path
           slurp
           util/parse-yaml
           (util/validate! ::customers)
           (map merge-defaults)
           (map transform)
           (util/validate! ::customers))
      (util/warn "customers file missing"))))
