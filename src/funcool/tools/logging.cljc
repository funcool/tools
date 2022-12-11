;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) Andrey Antukh

(ns funcool.tools.logging
  #?(:cljs (:require-macros [funcool.tools.logging]))
  (:require
   #?(:clj  [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])
   [clojure.spec.alpha :as s]
   [cuerdas.core :as str]
   [fipp.edn :as fpp]
   [funcool.tools.exceptions :as ex]
   [promesa.exec :as px]
   [promesa.util :as pu])
  #?(:clj
     (:import
      org.slf4j.LoggerFactory
      org.slf4j.Logger)))

#?(:clj (set! *warn-on-reflection* true))

(def ^{:doc "A global log-record atom instance; stores last logged record."}
  log-record
  (atom nil))

(def ^{:doc "Default executor instance used for processing logs."
       :dynamic true}
  *default-executor*
  (delay
    #?(:clj  (px/single-executor :factory (px/thread-factory :name "funcool.tools/logger"))
       :cljs (px/microtask-executor))))

#?(:cljs
   (goog-define props-format ":default")
   :clj
   (def ^:private props-format
     (System/getProperty "funcool.tools.logging.props-format" ":default")))

#?(:cljs
   (goog-define exception-format ":default")
   :clj
   (def ^:private exception-format
     (System/getProperty "funcool.tools.logging.exception-format" ":default")))

(def ^{:doc "Default format used to encode the log record props."
       :dynamic true}
  *default-props-format*
  (or (ex/try! (edn/read-string props-format)) :default))

(def ^{:doc "Default format used to encode the log cause exception."
       :dynamic true}
  *default-exception-format*
  (or (ex/try! (edn/read-string exception-format)) :default))

#?(:cljs
   (defonce ^:private loggers (js/Map.)))

#?(:cljs
   (declare ^:private level->int))

#?(:cljs
   (defn- get-logger-level
     "Get the current level set for the specified logger. Returns int."
     [^string logger]
     (letfn [(get-parent [logger]
               (let [lindex (.lastIndexOf ^string logger ".")]
                 (.slice ^string logger 0 (js/Math.max lindex 0))))]
       (let [val (.get ^js/Map loggers logger)]
         (if (pos? val)
           val
           (loop [logger' (get-parent logger)]
             (let [val (.get ^js/Map loggers logger')]
               (if (some? val)
                 (do
                   (.set ^js/Map loggers logger val)
                   val)
                 (if (= "" logger')
                   (do
                     (.set ^js/Map loggers logger 100)
                     100)
                   (recur (get-parent logger')))))))))))

(defn enabled?
  "Check if logger has enabled logging for given level."
  [logger level]
  #?(:clj
     (case level
       :trace (.isTraceEnabled ^Logger logger)
       :debug (.isDebugEnabled ^Logger logger)
       :info  (.isInfoEnabled  ^Logger logger)
       :warn  (.isWarnEnabled  ^Logger logger)
       :error (.isErrorEnabled ^Logger logger)
       :fatal (.isErrorEnabled ^Logger logger)
       (throw (IllegalArgumentException. (str "invalid level:"  level))))
     :cljs
     (>= (level->int level)
         (get-logger-level logger))))

(def ^:private reserved-props
  #{::level :cause ::logger ::sync? ::context})

(def ^:no-doc msg-props-xf
  (comp (partition-all 2)
        (map vec)
        (remove (fn [[k _]] (contains? reserved-props k)))))

(defmacro log!
  "Emit a new log record to the global log-record state (asynchronously). "
  [& props]
  (let [{:keys [::level ::logger ::context ::sync? cause] :or {sync? false}} props
        props (into [] msg-props-xf props)]
    `(let [props# (delay ~props)
           props# (if ~sync? @props# props#)]
       (px/run! *default-executor*
                (fn []
                  (let [lrecord# {::props props#
                                  ::context ~context
                                  ::level ~level
                                  ::logger ~logger
                                  ::cause ~cause}]
                    (swap! log-record (constantly lrecord#))))))))

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
  {:no-doc true}
  [props cause]
  (let [sbody  (fmt-props *default-props-format* props)
        scause (some->> cause (fmt-exception *default-exception-format*))]
    (cond-> sbody
      (some? scause)
      (str "\n" scause))))

#?(:clj
   (defn slf4j-log-handler
     {:no-doc true}
     [_ _ _ {:keys [::logger ::level ::props ::cause]}]
     (let [logger (LoggerFactory/getLogger ^String logger)]
       (when (enabled? logger level)
         (let [props   (if (delay? props) @props props)
               message (build-message props cause)]
           (case level
             :trace (.trace ^Logger logger ^String message ^Throwable cause)
             :debug (.debug ^Logger logger ^String message ^Throwable cause)
             :info  (.info  ^Logger logger ^String message ^Throwable cause)
             :warn  (.warn  ^Logger logger ^String message ^Throwable cause)
             :error (.error ^Logger logger ^String message ^Throwable cause)
             :fatal (.error ^Logger logger ^String message ^Throwable cause)
             (throw (IllegalArgumentException. (str "invalid level:"  level)))))))))

;; Attach asynchronous logs reader that forward all messages to SLF4J
#?(:clj (add-watch log-record ::default slf4j-log-handler))

#?(:cljs
   (def ^:private colors-map
     #js {:gray3  "#8e908c"
          :gray4  "#969896"
          :gray5  "#4d4d4c"
          :gray6  "#282a2e"
          :black  "#1d1f21"
          :red    "#c82829"
          :blue   "#4271ae"
          :orange "#f5871f"}))

#?(:cljs
   (defn- level->color
     [level]
     (case level
       (:error :err)         (unchecked-get colors-map "red")
       (:warn :warning :wrn) (unchecked-get colors-map "orange")
       (:info :inf)          (unchecked-get colors-map "blue")
       (:debug :dbg)         (unchecked-get colors-map "gray4")
       (:trace :trc)         (unchecked-get colors-map "gray3"))))

#?(:cljs
   (defn- level->name
     [level]
     (case level
       :debug "DBG"
       :trace "TRC"
       :info "INF"
       :warn "WRN"
       :warning "WRN"
       :error "ERR")))

#?(:cljs
   (defn- level->int
     [level]
     (case level
       :debug 10
       :trace 20
       :info 30
       :warn 40
       :warning 50
       :error 60)))

#?(:cljs
   (defn- prepare-message
     [props]
     (loop [props     (seq props)
            msg-props []
            oth-props []]
       (if (nil? props)
         [(fmt-props *default-props-format* msg-props) oth-props]
         (let [[k v :as kv] (first props)]
           (if (and (qualified-ident? k)
                    (= "js" (namespace k)))
             (recur (next props)
                    msg-props
                    (conj oth-props [:js (name k) (if (object? v) v (clj->js v))]))
             (recur (next props)
                    (conj msg-props kv)
                    oth-props)))))))

#?(:cljs
   (defn console-log-handler
     {:no-doc true}
     [_ _ _ {:keys [::logger ::props ::level ::cause]}]
     (when (enabled? logger level)
       (let [hstyles   (str/ffmt "font-weight: 600; color: %" (level->color level))
             styles    (str/ffmt "font-weight: 300; color: %" (unchecked-get colors-map "gray6"))
             header    (str "%c" (level->name level) " [" logger "] ")
             props     (if (delay? props) @props props)
             [msg adt] (prepare-message props)
             message   (str/concat header "%c" msg)]

         (js/console.group message hstyles styles)
         (doseq [[type n v] adt]
           (case type
             :js (js/console.log n v)
             :error (if (ex/info? v)
                      (js/console.error (pr-str v))
                      (js/console.error v))))


         (when cause
           (let [data    (ex-data cause)
                 explain (ex/explain data)]
             (when explain
               (js/console.log "Explain:")
               (js/console.log explain))

             (when (and data (not explain))
               (js/console.log "Data:")
               (js/console.log (ex/pprint-str data)))

             (js/console.log (.-stack cause))))

         (js/console.groupEnd message)))))

#?(:cljs (add-watch log-record ::default console-log-handler))

#?(:cljs
   (defn set-level*
     "Set the level (a keyword) of the given logger, identified by name."
     {:no-doc true}
     [name lvl]
     (.set ^js/Map loggers name (level->int lvl))))

(defmacro set-level!
  ([level]
   (when (:ns &env)
     `(set-level* ~(str *ns*) ~level)))
  ([n level]
   (when (:ns &env)
     `(set-level* ~n ~level))))

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
