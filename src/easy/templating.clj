(ns easy.templating
  "This namespace provides the interface to all things templating."
  (:require [hbs.core :as hbs]
            [hbs.helper :as hbsh :refer [defhelper safe-str register-helper! param]]
            [easy.util :as util]
            [clojure.string :as str]))

;; TODO: use https://github.com/leapfrogtechnology/just-handlebars-helpers because it has sprintf

;;; helpers

(def registry (hbs/registry (hbs/file-loader "" "")))

(defhelper uuid-helper [ctx options]
  (safe-str (str (random-uuid))))

(register-helper! registry "uuid" uuid-helper)

(defhelper add-helper [ctx options]
  (safe-str (+ ctx (param options 0))))

(register-helper! registry "add" add-helper)

(defhelper to-fixed-helper [ctx options]
  (safe-str (util/format (str "%." (param options 0) "f") (float ctx))))

(register-helper! registry "toFixed" to-fixed-helper)

(defhelper multiply-helper [ctx options]
  (safe-str (* ctx (param options 0))))

(register-helper! registry "multiply" multiply-helper)

(defhelper replace-helper [ctx options]
  (safe-str (str/replace ctx (param options 0) (param options 1))))

(register-helper! registry "replace" replace-helper)

;; rendering

;; TODO: don't read and parse the same template over and over again
(defn- apply-template [template-key event]
  ;; (println (:type event) (:source event) (template-key event))
  (let [path (template-key event)
        ;; _ (println "hello" path)
        source (util/slurp path)]
    (hbs/render registry source event)))

(def render-ledger
  (partial apply-template :ledger-template))

(def render-latex
  (partial apply-template :latex-template))

(def render-report
  (partial apply-template :report-template))

(def template (partial hbs/render registry))

(def render-overview
  (partial apply-template :overview-template))
