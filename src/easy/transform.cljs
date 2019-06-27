(ns easy.transform
  "The transform function is no only a multimethod but also
  overloaded (i.e. has multiple arities). Transform has to has its own
  namespace to avoid cyclic dependencies."
  (:require [easy.util :as util]))


(defmulti transform
  "Events will be transformed based on their type."
  (fn [_ e] (-> e :type keyword)))


(defmethod transform :default [_ event]
  (util/warn (str "WARNING: No method in multimethod "
                  "'easy.transform/transform' for dispatch value: "
                  (-> event :type keyword)
                  ", in event "
                  (prn-str event)))
  event)
