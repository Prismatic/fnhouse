(ns fnhouse.middleware
  "Middleware for coercing and schema-validating inputs and outputs of the API."
  (:use plumbing.core)
  (:require
   [schema.coerce :as coerce]
   [schema.core :as s]
   [schema.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

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
  (letk [[responses] handler-info]
    (let [coercion-matcher (bind-request output-coercer)
          response-walkers (map-vals
                            (fn [resp-schema]
                              (assert (contains? resp-schema :body)
                                      (format "Response %s for %s missing body"
                                              resp-schema handler-info))
                              (error-wrap
                               :response
                               (coerce/coercer resp-schema coercion-matcher)))
                            responses)]
      (fn [request response]
        (update-in response [:body]
                   (partial (safe-get response-walkers (response :status 200))
                            request)))))


  #_  (coerce/coercer (safe-get handler-info :responses) (bind-request output-coercer))

  )

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
      {:info info
       :handler
       (fn [request]

         (let [walked-request (request-walker request)
               handled-request (handler walked-request)
               walked-response (response-walker walked-request handled-request)]
           (println "walked-response " walked-response)
           walked-response))})))
