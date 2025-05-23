(ns easy.dctd
  "Diner's Club Transaction Details"
  (:require [clojure.spec.alpha :as s]
            [easy.util :as util :refer [assoc*]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clj-time.format :as format]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.log :as log]
            [easy.common :as common]))

;;; spec

(def wellformed-source? (partial re-matches #"[a-zA-Z0-9:-]+"))

(s/def ::type #{"dctd"})
(s/def ::rules string?) ;; TODO: an existing yml file
(s/def ::source (s/and string? wellformed-source?))
(s/def ::path string?) ;; TODO: an existing directory

(s/def ::event (s/keys :req-un [::type
                                ::rules
                                ::source
                                ::path]))

(s/def ::iso-date string?)
(s/def ::source-path string?)
(s/def ::index string?)
(s/def ::note string?)
(s/def ::description string?)
(s/def ::target string?)
(s/def ::amount number?)

(s/def ::transaction-event (s/keys :req-un [::iso-date
                                            ::source-path
                                            ::index
                                            ::description
                                            ::source
                                            ::target
                                            ::amount]
                                   :opt-un [::note]))

(s/def ::transaction-events (s/coll-of ::transaction-event))

;;; defaults

(def defaults
  {:pattern "^.*\\.csv$"
   :payer "Joint"})

(def merge-defaults
  (partial merge defaults))

;;; transformers

(defn add-mappings [evt]
  (->> evt
       :rules
       slurp
       util/parse-yaml
       (assoc* evt :mappings)))

(defn add-files [evt]
  (->> evt
       :path
       util/file-seq
       (filter #(re-matches (re-pattern (:pattern evt)) %))
       (assoc* evt :files)))

(defn add-csvs [evt]
  (->> evt
       :files
       (map util/read-csv)
       (assoc evt :csvs)))

(defn prepare-header [header]
  (-> header
      str/lower-case
      (str/replace #" " "-")
      keyword))

(def formatters-in [(format/formatter "MM.dd.yyyy")
                    (format/formatter "MM/dd/yyyy")])

(def formatter-out (format/formatter "yyyy-MM-dd"))

(defn fix-date [date]
  (loop [[format & remaining] formatters-in]
    (if (nil? format)
      date ;; giving up
      (if-let [result (try
                        (->> date
                             (format/parse format)
                             (format/unparse formatter-out))
                        (catch Throwable e nil))]
        result
        ;; if it failed try the next once
        (recur remaining)))))

(defn default-target [evt booking]
  (if (= "PENALTY INTEREST" (:contractual-partner booking))
    (:penalty-target evt)
    (:source evt)))

(defn fix-amount
  "From 2023-01-01 on it seems the column AMOUNT is only populated when
  there is a conversion, otherwise we have to resort to using column
  ORIGINAL-TRANSACTION-AMOUNT. Some bookings event have no amount at
  all, here we fallback to 0."
  [amount {:keys [original-transaction-amount] :as booking}]
  (->> [amount original-transaction-amount "0"]
       (remove empty?)
       first
       util/parse-float))

(defn prepare-booking [{:keys [mappings] :as evt} booking]
  (-> booking
      (set/rename-keys {:contractual-partner :description
                        :transaction-date :iso-date})
      (update :amount fix-amount booking)
      (update :iso-date fix-date)
      (update :invoice-date fix-date)
      (update :due-date fix-date)
      (merge (get mappings (keyword (:virtual-card-number booking)) {:target (default-target evt booking)}))))

(defn boring? [[_ v]]
  (or (nil? v) (and (string? v) (empty? v))))

(defn drop-boring [m]
  (into {} (remove boring? m)))

(defn make-key [s]
  (-> s
      str/lower-case
      (str/replace #"[^\w]" "-")
      (str/replace #"-+" "-")
      keyword))

(def crucial-keys
  #{:contractual-partner
    :transaction-date
    :amount
    :original-transaction-amount
    :invoice-date
    :due-date
    :virtual-card-number})

(defn assert-crucial-keys! [keys]
  (let [missing (set/difference crucial-keys (set keys))]
    (assert (empty? missing)
            (str "The csv seems to be missing these crucial columns: " (prn-str missing)))))

(defn bookings-from-csv [csv]
  (let [[header & rows] csv
        keys (map make-key header)]
    (assert-crucial-keys! keys)
    (map (partial zipmap keys) rows)))

(defn add-bookings [evt]
  (let [entries (->> evt
                     :csvs
                     (map bookings-from-csv)
                     flatten)
        ;; how many digits do we need to represent all entries
        digits (-> entries count str count)
        ;; pad lower numbers with zeros for sortability
        template (str "%0" digits "d")]
    (->> entries
         (map (partial prepare-booking evt))
         (map drop-boring)
         (sort-by :date)
         (map-indexed #(assoc %2 :index (util/format template %1)))
         (assoc* evt :bookings))))

(defn add-bookings-count [evt]
  (->> evt
       :bookings
       count
       (assoc evt :bookings-count)))

(defn add-templates [evt]
  (->> [:templates :ledger :dctd]
       (get-in @config)
       (assoc* evt :ledger-template)))

(def conveyed-keys [:type
                    :source
                    :ledger-template
                    :payer
                    :source-path])

(defn build-transaction-events [evt]
  (map (partial merge (select-keys evt conveyed-keys)) (:bookings evt)))

(defmethod transform :dctd [context evt]
  (-> evt
      (common/validate! ::event)
      merge-defaults
      add-mappings
      add-files
      add-csvs
      add-bookings
      (dissoc :csvs)
      add-bookings-count
      add-templates
      build-transaction-events
      (common/validate! ::transaction-events)))
