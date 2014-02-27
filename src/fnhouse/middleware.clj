(ns fnhouse.middleware
  "Middleware for coercing and schema-validating inputs and outputs of the API."
  (:use plumbing.core)
  (:require
   [fnhouse.routes :as routes]
   [schema.coerce :as coerce]
   [schema.core :as s]
   [schema.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Privet

(defn allow-extra
  "Allow any number of additional keyword->anything in a map schema."
  [schema]
  (if (contains? schema s/Keyword)
    schema
    (assoc schema s/Keyword s/Any)))

(def ^:private ^:dynamic *request*
  "Dynamic var used to propagate the request into the walk.  I think this is necessary
   until we figure out how to further generalize the walk protocol."
  ::error)

(defn bind-request
  "Compile a function from schema to (fn [request parsed-json]) to data
   into a schema.coerce/CoercionMatcher that doesn't need a request, but must
   be called within error-wrap."
  [coercer]
  (memoize (fn [schema] (when-let [c (coercer schema)] #(c *request* %)))))

(defn error-wrap
  "Take a context key and walker, and produce a function that walks a datum and returns a
   successful walk or throws an error for a walk that fails validation."
  [context walker]
  (fn [request data]
    (let [res (binding [*request* request] (walker data))]
      (if-let [error (utils/error-val res)]
        (throw (ex-info
                (format "Request: [%s]<BR>==> Error: [%s]<BR>==> Context: [%s]"
                        (pr-str (select-keys request [:uri :query-string :body]))
                        (pr-str error)
                        context)
                {:type :schema-error
                 :error error}))
        res))))


(defn request-walker
  "Given resources from the service (determined by keys asked for in v2 middleware)
   and handler metadata, compile a function for coercing and validating inputs to this
   API method (uri-args, query-params, and body).  Coercion is extensible by defining
   an implementation of 'coercer' above for your function."
  [resources input-coercer handler-meta]
  (let [coercion-matcher (bind-request (fn [schema] (input-coercer schema resources)))
        string-matcher (coerce/first-matcher [coercion-matcher coerce/string-coercion-matcher])
        json-matcher (coerce/first-matcher [coercion-matcher coerce/json-coercion-matcher])
        method-walkers (map-vals
                        (fnk method->walkers [{query-params {}} {body nil} returns]
                          (for-map [[request-key coercer]
                                    (merge
                                     (map-vals #(coerce/coercer % string-matcher)
                                               {:uri-args (:uri-args handler-meta {})
                                                ;; TODO: make this a flag
                                                :query-params (allow-extra query-params)})
                                     (when body {:body (coerce/coercer body json-matcher)}))]
                            request-key
                            (error-wrap request-key coercer)))
                        (safe-get handler-meta :methods))]
    (fn [request]
      (reduce-kv
       (fn [request request-key walker]
         (update-in request [request-key] (partial walker request)))
       request
       (safe-get method-walkers (safe-get request :request-method))))))



;;; TODO: don't hardcode json coercion

(defn response-walker
  "Given resources from the service (determined by keys asked for in v2 middleware)
   and handler metadata, compile a function for coercing and validating responses from
   this API method.  Coercion is extensible by defining an implementation of
   'coercer' above for your function."
  [resources output-coercer handler-meta]
  (let [coercion-matcher (bind-request (fn [schema] (output-coercer schema resources)))
        method-walkers (map-vals
                        (fnk [returns]
                          (map-vals
                           (fn [resp-schema]
                             (assert (contains? resp-schema :body)
                                     (format "Response %s for %s missing body"
                                             resp-schema handler-meta))
                             (error-wrap
                              :response
                              ;; TODO: allow-extra; don't schematize stuff outside the body
                              (coerce/coercer (allow-extra resp-schema) coercion-matcher)))
                           returns))
                        (safe-get handler-meta :methods))]
    (fn [request response]
      ((safe-get-in method-walkers [(safe-get request :request-method) (response :status 200)])
       request response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(defn coercion-middleware
  "Coerce and validate inputs and outputs.  Use walkers to simultaneously coerce and validate
   inputs in a generous way (i.e., 1.0 in body will be cast to 1 and validate against a long
   schema), and outputs will be clientized to match the output schemas."
  [resources input-coercer output-coercer]
  (fn [handler]
    (let [request-walker (request-walker resources input-coercer (meta handler))
          response-walker (response-walker resources output-coercer (meta handler))]
      (routes/propagate-meta
       (fn [request]
         (let [walked-request (request-walker request)]
           (->> walked-request
                handler
                (response-walker walked-request))))
       handler))))
