(ns easy.common.tax
  (:require [cljs.spec.alpha :as s]
            [easy.config :refer [config]]
            [easy.util :as util :refer [assoc*]]
            [easy.log :as log]
            [easy.common :as common]))


;; specs


(s/def ::rate (s/and float? #(< 0 %) #(> 1 %)))
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
       (filter #(or (nil? (:since %)) (>= date (:since %))))
       (filter #(or (nil? (:until %)) (<= date (:until %))))
       assert-exactly-one
       :rate))


;; transformers


(defn add-period
  "The period is when the vat is due."
  [evt]
  (let [date (-> evt :date)
        year (.getFullYear date)
        semester (if (< (.getMonth date) 6) 1 2)
        period (str year "-H" semester)]
    (assoc* evt :period period)))
