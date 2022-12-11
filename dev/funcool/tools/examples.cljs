(ns funcool.tools.examples
  (:require
   [funcool.tools.logging :as l]
   [promesa.core :as p]
   [promesa.protocols :as pt]
   [promesa.exec :as px]
   [clojure.spec.alpha :as s]))

(enable-console-print!)

(l/set-level! :debug)

(s/def ::foo int?)
(s/def ::bar string?)
(s/def ::foobar
  (s/keys :req-un [::foo ::bar]))

(defn main
  [& args]
  (js/console.log "main" args))

(defn ^:dev/after-load after-load
  []
  (let [data (s/explain-data ::foobar {:a 1})]
    (l/info :hello "world")
    (l/error :hello "error"
             :js/sample #js {:a 1 :b 2}
             :cause (ex-info "assertion error" data))))


