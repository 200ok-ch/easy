(ns easy.common.invoice-no
  (:require [clojure.string :as str]
            [cljs.spec.alpha :as s]
            [easy.util :as util]))


;; spec


(def match-invoice-no (partial re-matches #"^\d+\.\d+\.\d+$"))

(s/def ::invoice-no (s/and string? match-invoice-no))

(s/def ::customer-id pos-int?)
(s/def ::number pos-int?) ;; sequence
(s/def ::version pos-int?)

(s/def ::with (s/or :joined (s/keys :req-un [::invoice-no])
                    :split (s/keys :req-un [::customer-id
                                            ::number
                                            ::version])))


;; transformers


(defn- add-invoice-no
  "Compile `invoice-no` from invoice details like `customer-id`,
  `number` and `version`, if given."
  [evt]
  (->> [:customer-id :number :version]
       (map evt)
       (str/join ".")
       (util/assoc* evt :invoice-no)))


(defn- add-invoice-no-details
  "Derive invoice details like `customer-id`, `number` and `version`
  from `invoice-no`, if given."
  [{:keys [invoice-no] :as evt}]
  (if invoice-no
    (->> (str/split invoice-no #"\.")
         (map int)
         (zipmap [:customer-id :number :version])
         (util/merge* evt))
    evt))


(def unify
  "Transform fn for events that attempts to provide both: a compiled
  `invoice-no` and invoice details like `customer-id`, `number` and
  `version`."
  (comp add-invoice-no
        add-invoice-no-details))
