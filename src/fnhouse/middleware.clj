(ns fnhouse.middleware
  "Middleware for coercing and schema-validating requests and responses.

   By default -- passing (constantly nil) for input-coercer and output-coercer --
   ordinary schema validation is applied, with default string coercion for input uri-args
   and query-params and json coercion for the body (see schema.coerce).  Schema
   validation errors will throw with a helpful error message.

   In addition, custom RequestRelativeCoercionMatchers can be passed for input and
   output coercion, which enable the coercion of custom types in the input and output.

   For examples, see the included 'examples/guesthouse' project."
  (:use plumbing.core)
  (:require
   [schema.coerce :as coerce]
   [schema.core :as s]
   [schema.utils :as utils]
   [fnhouse.schemas :as schemas]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Schemas

(s/defschema RequestRelativeCoercionMatcher
  "A coerce/CoercionMatcher whose data coercion function also takes the request.  Useful
   for, e.g., client-relative presentation rules, expanding relative urls, etc."
  (s/=> (s/maybe (s/=> s/Any schemas/Request s/Any)) schemas/Schema))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(def ^:dynamic *default-matchers*
  "Default coercion matchers for request keys/responses.  Can be rebound for now,
   better API for specifying matchers is TBD."
  {:uri-args [coerce/string-coercion-matcher]
   :query-params [coerce/string-coercion-matcher]
   :body [coerce/json-coercion-matcher]
   :custom [coerce/string-coercion-matcher]
   :response []})

(s/defn coercing-walker
  "Take a context key, schema, and custom matcher, and produce a walker
   that returns the datum or throws an error for validation failure."
  [context :- s/Keyword
   schema
   custom-matcher :- RequestRelativeCoercionMatcher]
  (with-local-vars [request-ref ::missing] ;; used to pass request through to custom coercers.
    (let [walker (->> (cons
                       (fn [schema] (when-let [c (custom-matcher schema)] #(c @request-ref %)))
                       (safe-get *default-matchers* context))
                      vec
                      coerce/first-matcher
                      (coerce/coercer schema))]
      (fn [request data]
        (let [res (with-bindings {request-ref request} (walker data))]
          (if-let [error (utils/error-val res)]
            (throw (ex-info
                    (format "Request: [%s]<BR>==> Error: [%s]<BR>==> Context: [%s]"
                            (pr-str (select-keys request [:uri :query-string :body :custom]))
                            (pr-str error)
                            context)
                    {:type :coercion-error
                     :schema schema
                     :data data
                     :error error
                     :context context}))

            res))))))

(defn request-walker
  "Given a custom input coercer, compile a function for coercing and
   validating requests (uri-args, query-params, and body)."
  [input-coercer handler-info]
  (let [request-walkers (for-map [k [:uri-args :query-params :body :custom]
                                  :let [schema (safe-get-in handler-info [:request k])]
                                  :when schema]
                          k
                          (coercing-walker k schema input-coercer))]
    (fn [request]
      (reduce-kv
       (fn [request request-key walker]
         (update-in request [request-key] (partial walker request)))
       request
       request-walkers))))

(defn response-walker
  "Given a custom output coercer, compile a function for coercing and
   validating response bodies.  Other parts of the response map are not
   validated."
  [output-coercer handler-info]
  (let [response-walkers (map-vals (fn [s] (coercing-walker :response s output-coercer))
                                   (safe-get handler-info :responses))]
    (fn [request response]
      (let [walker (safe-get response-walkers (response :status 200))]
        (update-in response [:body] (partial walker request))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn coercion-middleware :- schemas/AnnotatedHandler
  "Coerce and validate inputs and outputs.  Use walkers to
   simultaneously coerce and validate inputs in a generous way (i.e.,
   1.0 in body will be cast to 1 in order to validate against a long
   schema), and outputs will be clientized to match the output schemas
   as specified by output-coercer.  If custom coercion is not needed,
   (constantly nil) be passed as a no-op coercer."
  [{:keys [handler info]} :- schemas/AnnotatedHandler
   input-coercer :- RequestRelativeCoercionMatcher
   output-coercer :- RequestRelativeCoercionMatcher]
  (let [request-walker (request-walker input-coercer info)
        response-walker (response-walker output-coercer info)]
    {:info info
     :handler (fn [request]
                (let [walked-request (request-walker request)]
                  (->> walked-request
                       handler
                       (response-walker walked-request))))}))
