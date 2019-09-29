(ns easy.transform
  "The transform function is no only a multimethod but also
  overloaded (i.e. has multiple arities). Transform has to has its own
  namespace to avoid cyclic dependencies."
  (:require [easy.util :as util]
            [easy.config :refer [config]]))


(defmulti transform
  "Events will be transformed based on their type."
  (fn [_ e] (-> e :type keyword)))


(defmethod transform :default [_ event]
  (util/warn (str "WARNING: No method in multimethod "
                  "'easy.transform/transform' for dispatch value: "
                  (-> event :type keyword)
                  ", in event "
                  (prn-str event)))
  ;; TODO: use default transformation!
  event)


;; silently ignoring transformation of nil
(defmethod transform nil [_ _]
  nil)


(defn safe-transform [ctx evt]
  (if (-> @config :options :options :debug)
    (prn (str "TRANSFORM " (:type evt))))
  (try
    (transform ctx evt)
    (catch :default e
      (util/warn (str "Transform failed with `"
                      e "` on\n\n" (util/write-yaml evt)
                      "\n" (.-stack e)))
      (process.exit 1) ;; halt on error
      evt)))
