(ns guesthouse.core
  "This is an example project that ties together all of the pieces of fnhouse:
    1. start with a namespace ('guesthouse.guestbook)
    2. suck up the handlers in the namespace with fnhouse.handlers library
    3. wrap each of them with schema coercion middleware from fnhouse.middleware
    4. compile all the handlers into a single router with fnhouse.routes
    5. wrap this root handler in standard ring middleware
    6. start a jetty server"
  (:use plumbing.core)
  (:require
   [clojure.string :as str]
   [ring.adapter.jetty :as jetty]
   [fnhouse.handlers :as handlers]
   [fnhouse.middleware :as middleware]
   [fnhouse.routes :as routes]
   [guesthouse.guestbook :as guestbook]
   [guesthouse.ring :as ring]
   [guesthouse.schemas :as schemas]))

(set! *warn-on-reflection* true)

(def entry-coercer
  "A custom output coercer that is used to coerce a server-side Entry
   into a ClientEntry whenever it appears in a response"
  (fn [schema]
    (when (= schema schemas/ClientEntry)
      (fn [request x]
        (let [[first last] (str/split (:name x) #" ")]
          (-> x
              (dissoc :name)
              (assoc :first-name first
                     :last-name last)))))))

(defn custom-coercion-middleware
  "Wrap a handler with the schema coercing middleware"
  [handler]
  (middleware/coercion-middleware
   handler
   (constantly nil)
   entry-coercer))

(defn wrapped-root-handler
  "Take the resources, partially apply them to the handlers in
   the 'guesthouse.guestbook namespace, wrap each with a custom
   coercing middleware, and then compile them into a root handler
   that will route requests to the appropriate underlying handler.
   Then, wrap the root handler in some standard ring middleware."
  [resources]
  (->> resources
       ((handlers/nss->handlers-fn {"guestbook" 'guesthouse.guestbook}))
       (map custom-coercion-middleware)
       routes/root-handler
       ring/ring-middleware))

(defn start-api
  "Take resources and server options, and spin up a server with jetty"
  [resources options]
  (-> resources
      wrapped-root-handler
      (jetty/run-jetty options)))

(set! *warn-on-reflection* false)