(ns hayride.core
  (:use plumbing.core)
  (:require
   [fnhouse.handlers :as handlers]
   [clojure.string :as str]
   [ring.middleware.json :as json]
   [ring.middleware.params :as params]
   [ring.middleware.keyword-params :as keyword-params]
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
- something fnhouse themed

to find:
- middleware for json stuff
"


(defn wrap-exception [f]
  (fn [request]
    (try (f request)
         (catch Exception e
           (.printStackTrace e)
           {:status 500
            :body "Exception caught"}))))

(defn spy-request [f]
  (fn [request]
    (println "SPY REQUEST: " request)
    (f request)))

(defn keywordize-middleware [handler]
  (fn [req]
    (handler
     (update-in req [:query-params] keywordize-map))))

(defnk start-api [handlers & opts]
  (let [middleware (middleware/coercion-middleware (constantly nil) (constantly nil))]
    (jetty/run-jetty
     (-> (routes/root-handler (map middleware handlers))
         spy-request
         keywordize-middleware
         (json/wrap-json-body {:keywords? true})
         params/wrap-params
         json/wrap-json-response
         wrap-exception)
     opts)))
