(ns easy.common.tax
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.walk :as walk]
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

;; (defn unordered [ordered]
;;   (walk/postwalk (fn [x] (if (map? x) (into {} x) x)) ordered))
;;
;; (defn- assert-exactly-one [coll]
;;   (case (count coll)
;;     ;; non found -> abort
;;     0 (util/die "Collection with one expected entry found empty!")
;;     ;; all good
;;     1 (first coll)
;;     ;; else (more than 1)
;;     (do
;;       (util/warn (str "Found more than one entry in collection, "
;;                       "when exactly one was expected. "
;;                       "Just picked the first from:"))
;;       (doseq [entry coll]
;;         (util/warn (str "- " (str/trim (prn-str (unordered entry))))))
;;       (first coll))))

(defn- applicable? [date {:keys [since until]}]
  ;; keep it
  (and
   (or
    ;; if it has no :since
    (nil? since)
    ;;or if the :since is before the event's date
    (time/before? since date))
   ;; and
   (or
    ;; if it has no :until
    (nil? until)
    ;; or :until is after the event's date
    (time/after? until date))))

(defn lookup-rate
  "Takes a KEY and an EVT"
  [key {:keys [date] :as evt}]
  (->> @config
       key
       ;; TODO: use `(common/validate! ::rates)` here
       (filter (partial applicable? date))
       ;; if there are mulitple the last :since should win
       (sort-by :since)
       last
       :rate))

(comment
  (require '[clj-time.format :as f])
  ;; is x before y
  (time/before? (f/parse "2024-01-01") (f/parse "2025-01-01"))
  )

;; transformers

(defn add-period
  "The period is when the vat is due."
  ([evt] (add-period evt [:date]))
  ([evt date-path]
   (if-let [date (get-in evt date-path)]
     (let [year (time/year date)
           semester (if (< (time/month date) 6) 1 2)
           period (str year "-H" semester)]
       (assoc* evt :period period))
     ;; no date found, in context of a nested transform, that's ok
     evt)))
