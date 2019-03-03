(ns easy.overview
  (:require [easy.config :refer [config]]))

(defn- unsettled-invoice [event]
  (and (= "revenue" (:type event))
       (nil? (:settled event))))

;;(defn- accumulate-debt [customer event]
;;  (-> customer
;;      (assoc :name (get-in event [:customer :name]))
;;      (update :sum + (:gross-total event))
;;      (update :events conj event)))
;;
;;(defn- accumulate-debt-reducer [result event]
;;  (let [id (:customer-id event)]
;;    (update result id accumulate-debt event)))
;;
;;(defn- top-creditors [events]
;;  (let [unsettled (unsettled-invoice event)]
;;    (reduce accumulate-debt-reducer {} unsettled)))

(defn crunch-numbers [events]
  ;;(println (top-creditors events))
  {:overview-template (get-in @config [:templates :output :overview])
   :unsettled (filter unsettled-invoice events)
   ;;:top-creditors (top-creditors events)
   })