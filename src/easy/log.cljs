(ns easy.log
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :refer [join replace]]
            [easy.config :refer [config]]))


(defn warn [msg]
  (.error js/console (clj->js msg))
  msg)


(defn debug [& args]
  (if (-> @config :options :options :debug)
    (->> args
         reverse
         (apply prn-str)
         warn))
  (first args))


;; (defn spy [& args]
;;   (apply pprint args)
;;   (last args))
