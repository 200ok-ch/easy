(ns easy.invoice.item
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]))

;;; spec

(s/def ::rate number?)
(s/def ::hours number?)
(s/def ::beneficiary string?)
(s/def ::description string?)
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

(s/def ::item (s/or :with-amount-and-description
                    (s/keys :req-un [::amount
                                     ::description
                                     ::beneficiary]
                            :opt-un [::hours
                                     ::rate
                                     ::timesheet
                                     ::timesheet-data])
                    :with-rate-and-hours
                    (s/keys :req-un [::rate
                                     ::hours
                                     ::beneficiary]
                            :opt-un [::timesheet
                                     ::timesheet-data])
                    :with-rate-and-timesheet
                    (s/keys :req-un [::rate
                                     ::timesheet
                                     ::beneficiary]
                            :opt-un [::timesheet-data])))

;;; defaults

(def defaults {})

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
    (let [sum (sum-time timesheet-data)]
      (-> item
          (assoc :sum-hours sum) ;; actual sum
          (assoc* :hours sum))) ;; this might be overridden
    item))

(defn- add-amount [{:keys [rate hours] :as item}]
  (if (and rate hours)
    (->> (* rate hours)
         util/round-currency
         (assoc* item :amount))
    item))

(defn- add-amount-with-discount-and-delcredere [item invoice]
  (->> (/ (invoice :discount) 100)
       (* (item :amount))
       (- (item :amount))
       (* 0.9) ;; the famous delcredere constant
       util/round-currency
       (assoc* item :amount-with-discount-and-delcredere)))

(defn- add-description [{:keys [hours addendum rate] :as item}]
  ;; TODO: make this default adjustable via config or templates
  ;; only add a description preset if we have the data to render something meaningful
  (if (and hours rate)
    (->> (str "Beratung " addendum " (" hours " x " rate " CHF/Std.)")
         (assoc* item :description))
    item))

(defn transform [item invoice]
  (-> item
      merge-defaults
      read-timesheet
      prepare-timesheet
      add-hours
      add-amount
      add-description
      (add-amount-with-discount-and-delcredere invoice)))
