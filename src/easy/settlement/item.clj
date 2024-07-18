(ns easy.settlement.item
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]))

;; TODO: This file has some code duplication with invoice items. In
;; fact if might not be needed at all! Because settlements get all
;; required "item" details from their associated invoice.

;;; spec

(s/def ::rate number?)
(s/def ::hours number?)
(s/def ::beneficiary string?)
(s/def ::discount number?)
(s/def ::amount number?)
(s/def ::timesheet (s/and string? #(.endsWith % ".csv")))

;; TODO: Write a spec for timesheet-data. It can look like this, e.g.
;;
;; (["date" "duration" "hashtags" "title" "id\r"]
;;  ["2019-01-16" "3" "#sva" "#sva" "4uuu4j65cltg6up20c8jmidul6@google.com\r"]
;;  ["2019-01-28" "3" "#sva" "#sva" "04bss4v77o1olpkssakhgdr0q2@google.com"])
;;
;; (s/def ::timesheet-data

;; TODO: Write a spec for timesheet-prepared. It can look like this, e.g.
;;
;; [{:date "2019-01-16" :duration "3" :description "#sva"}]
;;
;; (s/def ::timesheet-prepared

;; TODO: how do I spec: either hours or timesheet has to be present?

(s/def ::item (s/keys :req-un [::rate
                               ::beneficiary]
                      :opt-un [::hours
                               ::timesheet
                               ::timesheet-data
                               ::discount
                               ::amount]))

;;; defaults

(def defaults
  {:discount 0})

(def ^:private merge-defaults
  (partial merge defaults))

;;; helpers

(defn- read-timesheet [item]
  (if-let [timesheet (:timesheet item)]
    (->> timesheet
         util/read-csv
         (assoc* item :timesheet-data))
    item))

(defn- prepare-timesheet [item]
  (if-let [data (:timesheet-data item)]
    (->> data
         (drop 1)
         ;; TODO: refactor these out into a transform-timesheet-entry
         (map #(take 4 %))
         (map #(interleave [:date :duration :hashtags :description] %))
         (map #(apply hash-map %))
         (map #(update % :description util/sanitize-latex))
         (assoc* item :timesheet-prepared))
    item))

(defn- sum-time [timesheet-data]
  (->> timesheet-data
       (drop 1) ;; drop header
       (map second) ;; use 2nd column
       (map util/parse-float) ;; coerce to float
       (reduce +))) ;; sum

;;; transformers

(defn- add-hours [item]
  (if-let [timesheet-data (:timesheet-data item)]
    (assoc* item :hours (sum-time timesheet-data))
    item))

(defn- add-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       ;; TODO: calculate and subtract discount
       util/round-currency
       (assoc* item :amount)))

(defn transform [item]
  (-> item
      merge-defaults
      read-timesheet
      prepare-timesheet
      add-hours
      add-amount))
