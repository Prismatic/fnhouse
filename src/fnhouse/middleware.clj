(ns fnhouse.middleware
  "Middleware for coercing and schema-validating inputs and outputs of the API."
  (:use plumbing.core)
  (:require
   [schema.coerce :as coerce]
   [schema.core :as s]
   [schema.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

;; annotated-handler -> handler

;; (handler-info, handler) + behavior

;; => output-request-data => response

;; resources -> response-schema -> input-response-data -> output-ouptput-data



;; coercer...
;; (coerce/coercer schema coercer-fn) -> data -> coerced-data-or-error




;; goes away
(defn allow-extra
  "Allow any number of additional keyword->anything in a map schema."
  [schema]
  (if (contains? schema s/Keyword)
    schema
    (assoc schema s/Keyword s/Any)))

(def ^:private ^:dynamic *request*
  "Dynamic var used to propagate the request into the walk."
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
  "Given resources from the service and handler metadata, compile a
   function for coercing and validating inputs (uri-args,
   query-params, and body).  Coercion is extensible by defining an
   implementation of 'coercer' above for your function."
  [input-coercer handler-info]
  (letk [[[:request body :as request]] handler-info]
    (let [coercion-matcher (bind-request input-coercer)
          string-matcher (coerce/first-matcher [coercion-matcher coerce/string-coercion-matcher])
          json-matcher (coerce/first-matcher [coercion-matcher coerce/json-coercion-matcher])
          walker (for-map [[request-key coercer]
                           (merge (map-vals #(coerce/coercer % string-matcher)
                                            (select-keys request [:uri-args :query-params]))
                                  {:body (when body (coerce/coercer body json-matcher))})]
                   request-key
                   (error-wrap request-key coercer))]
      (fn [request]
        (reduce-kv
         (fn [request request-key walker]
           (update-in request [request-key] (partial walker request)))
         request
         walker)))))



;;; TODO: don't hardcode json coercion

(defn response-walker
  "Given resources from the service (determined by keys asked for in v2 middleware)
   and handler metadata, compile a function for coercing and validating responses from
   this API method.  Coercion is extensible by defining an implementation of
   'coercer' above for your function."
  [output-coercer handler-info]
  (coerce/coercer (safe-get handler-info :responses) (bind-request output-coercer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(defn coercion-middleware
  "Coerce and validate inputs and outputs.  Use walkers to simultaneously coerce and validate
   inputs in a generous way (i.e., 1.0 in body will be cast to 1 and validate against a long
   schema), and outputs will be clientized to match the output schemas."
  [input-coercer output-coercer]
  (fnk [handler info :as annotated-handler]
    (let [request-walker (request-walker input-coercer info)
          response-walker (response-walker output-coercer info)]
      (fn [request]
        (let [walked-request (request-walker request)]
          (->> walked-request
               handler
               (response-walker walked-request))))
      annotated-handler)))
