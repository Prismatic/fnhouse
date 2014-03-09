(ns hayride.core
  (:use plumbing.core)
  (:require
   [plumbing.graph :as graph]
   [fnhouse.core :as fnhouse]
   [fnhouse.handlers :as handlers]
   [clojure.string :as str]
   [ring.middleware.json :as json]
   [ring.middleware.params :as params]
   [fnhouse.routes :as routes]
   [fnhouse.middleware :as middleware]
   [ring.adapter.jetty :as jetty])
  (:import
   [org.mortbay.jetty Server]
   [org.mortbay.jetty.nio SelectChannelConnector]))

(set! *warn-on-reflection* true)

#_
"
input coercion
output coercion
custom input/output coercion
doc generation
non-trivial resources
nesting of subgraphs
   (instance)
   (maybe use Leon's )
   maybe release other stuff (in plumbing or separate)
URI args
query params
bodies in/out

include nice exception printing (e.g. for schema errors)


possible example applications:
- todos
- twitter clone
- somehting fnhouse themed

to find:
- middleware for json stuff
"

(defn- create-server
  "Construct a Jetty Server instance."
  [options]
  (let [connector (doto (SelectChannelConnector.)
                    (.setPort (options :port 80))
                    (.setHost (options :host))
                    (.setAcceptQueueSize 256))
        server    (doto (Server.)
                    (.addConnector connector)
                    (.setSendDateHeader true))]
    (when-let [t (:shutdown-timeout options)]
      (.setGracefulShutdown server (int t)))
    (when-let [n (:num-threads options)]
      (println "Using fixed-size thread pool %d" n)
      (doto server
        (.setThreadPool (org.mortbay.thread.QueuedThreadPool. (int n)))))
    server))


(defn ^Server run-jetty
  "Serve the given handler according to the options.
  Options:
    :configurator   - A function called with the Server instance.
    :port
    :num-threads    - num threads for fixed pool
    :host
    :join?          - Block the caller: defaults to true.
    :ssl?           - Use SSL.
    :ssl-port       - SSL port: defaults to 443, implies :ssl?
    :keystore
    :key-password
    :truststore
    :trust-password
    :shutdown-timeout - Allow requests to finish for this many ms."
  [handler options]
  (let [^Server s (create-server (dissoc options :configurator))]
    (when-let [configurator (:configurator options)]
      (configurator s))
    (doto s
      (.addHandler (@#'jetty/proxy-handler handler))
      (.start))
    (when (:join? options true)
      (.join s))
    s))

(defnk server-resource [root-handler & opts]
  (org.mortbay.log.Log/setLog nil)
  (run-jetty root-handler opts))

(defn parse-query-params-middleware [handler]
  (fn [request]
    (let [query-string (:query-string request)]
      (-> request
          (assoc :query-params
            (for-map [[k v] (map #(str/split % #"=") (str/split query-string #"&"))]
              (keyword k) v))
          handler))))


(defnk start-api [handlers {observer nil} & opts]
  (let [middleware (middleware/coercion-middleware (constantly nil) (constantly nil))]
    (server-resource
     (assoc opts
       :root-handler
       (->> handlers
            (map middleware)
            routes/root-handler
            json/wrap-json-response)))))
