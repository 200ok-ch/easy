(ns easy.dctd
  "Diner's Club Transaction Details"
  (:require [cljs.spec.alpha :as s]
            [easy.util :as util :refer [assoc* assert-only-one!]]
            [lumo.util :as lumo]
            [goog.labs.format.csv :as csv]
            [clojure.string :as str]
            [clojure.set :as set]
            [cljs-time.core :as cljs-time]
            [cljs-time.format :as time]
            [easy.config :refer [config]]
            [easy.transform :refer [transform]]
            [easy.log :as log]
            [easy.common :as common]))

;; spec

(def wellformed-source? (partial re-matches #"[a-zA-Z0-9:-]+"))

(s/def ::type #{"dctd"})
(s/def ::rules string?) ;; TODO: an existing yml file
(s/def ::source (s/and string? wellformed-source?))
(s/def ::path string?) ;; TODO: an existing directory

(s/def ::event (s/keys :req-un [::type
                                ::rules
                                ::source
                                ::path]))

;; TODO: write a spec based on the ledger template
(s/def ::transaction-event (s/keys :req-un []))

(s/def ::transaction-events (s/coll-of ::transaction-event))

;; defaults

(def defaults
  {:pattern "^.*\\.csv$"
   :payer "Joint"})

(def merge-defaults
  (partial merge defaults))

;; transformers

(defn add-mappings [evt]
  (->> evt
       :rules
       util/slurp
       util/parse-yaml
       (assoc* evt :mappings)))

(defn add-files [evt]
  (->> evt
       :path
       lumo/file-seq
       (filter #(re-matches (re-pattern (:pattern evt)) %))
       (assoc* evt :files)))

(defn add-csvs [evt]
  (->> evt
       :files
       (map util/slurp)
       (map csv/parse)
       (map js->clj)
       (assoc evt :csvs)))

(defn prepare-header [header]
  (-> header
      str/lower-case
      (str/replace #" " "-")
      keyword))

(defn add-header [evt]
  (->> evt
       :csvs
       (map first)
       distinct
       (assert-only-one! "Failed expectation: All csv headers are the same.")
       first
       (map prepare-header)
       (assoc* evt :header)))

(defn read-bookings [file]
  (->> file
       util/slurp
       csv/parse
       js->clj
       (drop 1)))

(def formatter-in (time/formatter "MM/dd/yyyy"))

(def formatter-out (time/formatter "yyyy-MM-dd"))

(defn fix-date [date]
  (->> date
       (time/parse formatter-in)
       (time/unparse formatter-out)))

(defn default-target [evt booking]
  (if (= "PENALTY INTEREST" (:contractual-partner booking))
    (:penalty-target evt)
    (:source evt)))

(defn prepare-booking [{:keys [mappings] :as evt} booking]
  (-> booking
      (set/rename-keys {:contractual-partner :description
                        :transaction-date :iso-date})
      (update :amount js/parseFloat)
      (update :iso-date fix-date)
      (update :invoice-date fix-date)
      (update :due-date fix-date)
      (merge (get mappings (keyword (:virtual-card-number booking)) {:target (default-target evt booking)}))))

(defn boring? [[_ v]]
  (or (nil? v) (and (string? v) (empty? v))))

(defn drop-boring [m]
  (into {} (remove boring? m)))

(defn add-bookings [evt]
  (->> evt
       :csvs
       (map (partial drop 1))
       (apply concat)
       (map (partial zipmap (:header evt)))
       (map (partial prepare-booking evt))
       (map drop-boring)
       (sort-by :date)
       (map-indexed #(assoc %2 :index %1))
       (assoc* evt :bookings)))

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
      add-header
      add-bookings
      (dissoc :csvs)
      add-bookings-count
      add-templates
      build-transaction-events
      (common/validate! ::transaction-events)))
