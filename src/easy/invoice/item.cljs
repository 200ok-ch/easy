(ns easy.invoice.item
  (:require [cljs.spec.alpha :as s]
            [testdouble.cljs.csv :as csv]
            [easy.util :as util :refer [assoc*]]))


;; spec


(s/def ::rate float?)
(s/def ::hours float?)
(s/def ::beneficiary string?)
(s/def ::amount float?)
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
                               ::amount]))


;; defaults


(def defaults {})


(def ^:private merge-defaults
  (partial merge defaults))


;; helpers


(defn- read-timesheet [item]
  (if-let [timesheet (:timesheet item)]
    (->> timesheet
         util/slurp
         csv/read-csv
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
       (map js/parseFloat) ;; coerce to float
       (reduce +))) ;; sum


;; transformers


(defn- add-hours [item]
  (if-let [timesheet-data (:timesheet-data item)]
    (let [sum (sum-time timesheet-data)]
      (-> item
          (assoc :sum-hours sum) ;; actual sum
          (assoc* :hours sum))) ;; this might be overridden
    item))


(defn- add-amount [item]
  (->> (map item [:rate :hours])
       (apply *)
       util/round-currency
       (assoc* item :amount)))


(defn- add-amount-with-discount-and-delcredere [item invoice]
  (->> (/ (invoice :discount) 100)
       (* (item :amount))
       (- (item :amount))
       (* 0.9) ;; the famous delcredere constant
       util/round-currency
       (assoc* item :amount-with-discount-and-delcredere)))


(defn transform [item invoice]
  (-> item
      merge-defaults
      read-timesheet
      prepare-timesheet
      add-hours
      add-amount
      (add-amount-with-discount-and-delcredere invoice)))
