;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh

(ns funcool.tools.logging
  #?(:cljs (:require-macros [funcool.tools.logging]))
  (:require
   #?(:clj [clojure.edn :as edn])
   [funcool.tools.exceptions :as ex]
   [cuerdas.core :as str]
   [promesa.exec :as px]
   [promesa.exec.async-atom :as atm]
   [clojure.spec.alpha :as s])
  #?(:clj
     (:import
      org.slf4j.LoggerFactory
      org.slf4j.Logger)))

#?(:clj (set! *warn-on-reflection* true))

(declare build-message)

(def ^{:doc "A global logs async atom instance."}
  log-record
  (atm/atom nil))

(defn enabled?
  [logger level]
  (case level
    :trace (.isTraceEnabled ^Logger logger)
    :debug (.isDebugEnabled ^Logger logger)
    :info  (.isInfoEnabled  ^Logger logger)
    :warn  (.isWarnEnabled  ^Logger logger)
    :error (.isErrorEnabled ^Logger logger)
    :fatal (.isErrorEnabled ^Logger logger)
    (throw (IllegalArgumentException. (str "invalid level:"  level)))))

(defn write!
  [logger level message cause]
  (case level
    :trace (.trace ^Logger logger ^String message ^Throwable cause)
    :debug (.debug ^Logger logger ^String message ^Throwable cause)
    :info  (.info  ^Logger logger ^String message ^Throwable cause)
    :warn  (.warn  ^Logger logger ^String message ^Throwable cause)
    :error (.error ^Logger logger ^String message ^Throwable cause)
    :fatal (.error ^Logger logger ^String message ^Throwable cause)
    (throw (IllegalArgumentException. (str "invalid level:"  level)))))

#?(:cljs (defrecord Logger [id level]))

(def ^:private reserved-props
  #{::level :cause ::logger ::sync? ::context})

(def msg-props-xf
  (comp (partition-all 2)
        (map vec)
        (remove (fn [[k _]] (contains? reserved-props k)))))

(defmacro log!
  [& props]
  (let [{:keys [::level ::logger ::context ::sync? cause] :or {sync? false} :as props'} props
        props (into [] msg-props-xf props)]
    `(let [props# (delay ~props)
           props# (if ~sync? @props# props#)]
       (atm/send log-record
                 (fn [_#]
                   {::props props#
                    ::context ~context
                    ::level ~level
                    ::logger ~logger
                    ::cause ~cause})))))

#(:clj
  (def default-props-format
    (let [format (System/getProperty "funcool.tools.logging.format-props" ":default")]
      (edn/read-string format))))

#(:clj
  (def default-exception-format
    (let [format (System/getProperty "funcool.tools.logging.format-exception" ":default")]
      (edn/read-string format))))

(defmulti fmt-props (fn [t _] t))
(defmethod fmt-props :default
  [_ props]
  (loop [props  (seq props)
         result []]
    (if-let [[k v] (first props)]
      (recur (next props)
             (conj result (str (name k) "=" (pr-str v))))
      (str/join ", " result))))

(defmulti fmt-exception (fn [t _] t))
(defmethod fmt-exception :default
  [_ cause]
  #?(:clj  (with-out-str (ex/print-throwable cause))
     :cljs nil))

(defn build-message
  [props cause]
  (let [sbody  (fmt-props default-props-format props)
        scause (some->> cause (fmt-exception default-exception-format))]
    (cond-> sbody
      (some? scause)
      (str "\n" scause))))

#?(:clj
   (defn slf4j-log-handler
     [_ _ _ {:keys [::logger ::level ::props ::cause]}]
     (let [logger (LoggerFactory/getLogger ^String logger)]
       (when (enabled? logger level)
         (let [props   (if (delay? props) @props props)
               message (build-message props cause)]
           (write! logger level message cause))))))

;; Attach asynchronous logs reader that forward all messages to SLF4J
#?(:clj (add-watch log-record ::default slf4j-log-handler))

(defmacro info
  [& params]
  `(do
     (log! ::logger ~(str *ns*) ::level :info ~@params)
     nil))

(defmacro error
  [& params]
  `(do
     (log! ::logger ~(str *ns*) ::level :error ~@params)
     nil))

(defmacro warn
  [& params]
  `(do
     (log! ::logger ~(str *ns*) ::level :warn ~@params)
     nil))

(defmacro debug
  [& params]
  `(do
     (log! ::logger ~(str *ns*) ::level :debug ~@params)
     nil))

(defmacro trace
  [& params]
  `(do
     (log! ::logger ~(str *ns*) ::level :trace ~@params)
     nil))
