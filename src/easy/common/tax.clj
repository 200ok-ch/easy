(ns easy.common.tax
  (:require [clojure.spec.alpha :as s]
            [easy.config :refer [config]]
            [easy.util :as util :refer [assoc*]]
            [easy.log :as log]
            [easy.common :as common]
            [clj-time.core :as time]))

;; specs

(s/def ::rate (s/and number? #(< 0 %) #(> 1 %)))
(s/def ::since util/date?)
(s/def ::until util/date?)

(s/def ::rate-entry (s/keys :req-un [::rate]
                            :opt-un [::since
                                     ::until]))

(s/def ::rates (s/coll-of ::rate-entry :kind vector?))

;; helpers

(defn- assert-exactly-one [coll]
  (case (count coll)
    ;; non found -> abort
    0 (util/die "Collection with one expected entry found empty!")
    ;; all good
    1 (first coll)
    ;; else (more than 1)
    (do
      (util/warn (str "Found more than one entry in collection, "
                      "when exactly one was expected. "
                      "Just picked the first from " coll))
      (first coll))))

(defn lookup-rate
  [key {:keys [date]}]
  (->> @config
       key
       ;; TODO: use `(common/validate! ::rates)` here
       (filter #(or (nil? (:since %)) (not (time/before? date (:since %)))))
       (filter #(or (nil? (:until %)) (not (time/after? date (:until %)))))
       assert-exactly-one
       :rate))

;; transformers

(defn add-tax-period
  "The period is when the vat is due."
  ([evt] (add-tax-period evt [:date]))
  ([evt date-path]
   (if-let [date (get-in evt date-path)]
     (let [year (time/year date)
           semester (if (< (time/month date) 6) 1 2)
           period (str year "-H" semester)]
       (assoc* evt :tax-period period))
     ;; no date found, in context of a nested transform, that's ok
     evt)))
