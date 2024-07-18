(ns easy.log
  (:require [easy.config :refer [config]]))

(defn warn [msg]
  (binding [*out* *err*]
    (apply println msg))
  msg)

(defn debug [& args]
  (if (-> @config :options :options :debug)
    (->> args
         reverse
         (apply prn-str)
         warn))
  (first args))

(defn debug-evt [evt & args]
  (if (-> @config :options :options :debug)
    (println (str "[" (:invoice-no evt) "]") (apply str args))))
