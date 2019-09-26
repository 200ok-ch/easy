(ns easy.common.tax
  (:require [cljs.spec.alpha :as s]
            [easy.config :refer [config]]
            [easy.util :as util :refer [assoc*]]
            [easy.common :as common]))


;; specs


(s/def ::rate (s/and float? #(< 0 %) #(> 1 %)))
(s/def ::since util/date?)
(s/def ::until util/date?)

(s/def ::rate-entry (s/keys :req-un [::rate]
                            :opt-un [::since
                                     ::until]))

(s/def ::rates (s/coll-of ::rate-entry))


;; helpers


(defn lookup-rate
  [key {:keys [date]}]
  (->> @config
       key
       (common/validate! ::rates)
       (filter #(or (nil? (:since %)) (>= date (:since %))))
       (filter #(or (nil? (:until %)) (<= date (:until %))))
       ;; TODO: assert-exactly-one!
       first
       :rate))


;; transformers


(defn add-period
  "The period is when the vat is due."
  [evt]
  (let [date (-> evt :date)
        year (.getFullYear date)
        semester (if (< (.getMonth date) 6) 1 2)
        period (str year "-H" semester)]
    (assoc* evt :period)))
