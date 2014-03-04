(ns fnhouse.core
  "Utilities and helpers for converting namespaces of functions into handlers."
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clojure.string :as str]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [plumbing.map :as map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema

(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (clojure.pprint/pprint x#)
     x#))

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema RingRequest
  (map-keys
   s/optional-key
   {:uri-args KeywordMap
    :query-params KeywordMap
    :body s/Any}))

(s/defschema RingResponse
  s/Any)

(s/defschema Handler
  (s/=> RingResponse RingRequest))

(s/defschema Symbol
  (s/pred symbol? 'symbol?))

(s/defschema Var
  (s/pred var? 'var?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(defn propagate-meta
  "Copy the meta from one object into another without overwriting any existing fields."
  [to from]
  (vary-meta
   to
   (fn [to-meta from-meta] (merge from-meta to-meta))
   (meta from)))

(defn- handler-input [handler input]
  (safe-get (pfnk/input-schema handler) input))

(defn- handler-property [handler property]
  (safe-get (meta handler) property))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(s/defschema ResponseCode s/Int)
(s/defschema Schema (s/protocol s/Schema))

;; Custom Response Walking for validation based on status code

(s/defschema ResponseSchema
  {:description s/Str
   :code ResponseCode
   :body Schema})

(s/defn ^:always-validate response :- ResponseSchema
  [description code body]
  {:description description
   :code code
   :body body})

(s/defschema HandlerInfo
  (s/both
   {:path String
    :method (s/enum :get :head :post :put :delete)

    :short-description s/Str
    :description s/Str

    :uri-args {s/Keyword (s/protocol s/Schema)}
    :body (s/maybe s/Schema)
    :query-params {s/Keyword (s/protocol s/Schema)
                   (s/optional-key s/Keyword) Schema} ;; good luck


    ;; maybe include a per-response doc format
    :responses String ;;TODO: Responses

    :source-map (s/maybe
                 {:line s/Int
                  :column s/Int
                  :file s/Str
                  :ns s/Str
                  :name s/Str})

    :annotations s/Any
    ;; (private, auth level, etc etc ?? ? ??) (maybe just save a key in the meta?)
    ;; or maybe inject a fn for extracting to nss->route-spec


    ;; Would you guys want this stuff? we can provide it!
    ;; :resources ??? ;; schema map, useful?
    ;; :file/line/var/ns ;; github link
    ;; :full-request-schema???? ;; probably not useful  (stuff other than params).
    ;; :full-response-schema???? ;; also probably not useful (stuff other than response body)
    }
   (s/pred
    (fnk [method body]
      (= (boolean (#{:post :put} method))
         (boolean body)))
    'only-post-has-body?)))

(s/defn path [handler :- Handler]
  (get (meta handler) :path
       (->> [:route-prefix :name]
            (map #(handler-property handler %))
            (apply str))))

(s/defn uri-args [handler :- Handler]
  (handler-input handler :uri-args))

(s/defn query-params [handler :- Handler]
  (handler-input handler :query-params))

(s/defn body [handler :- Handler]
  (handler-input handler :body))

(s/defn private? [handler :- Handler]
  (boolean (get (meta handler) :private)))

(s/defn description [handler :- Handler]
  (handler-property handler :doc))

(s/defn short-description [handler :- Handler]
  (-> handler
      description
      (str/split #"\n" 2)
      first))

(s/defn responses [handler :- Handler]
  (pfnk/output-schema handler))

(s/defn compile-handler :- Handler
  "Partially apply the the handler to the resources and return a fnk from request to response"
  [resources handler]
  (propagate-meta
   (pfnk/fn->fnk
    (fn [request] (handler {:request request :resources resources}))
    [(:request (pfnk/input-schema handler) {})
     (pfnk/output-schema handler)])
   handler))

(s/defn compile-routes :- (s/=> [Handler] (s/named KeywordMap "resources"))
  "Take a seq of handlers and produce a single fnk from resources to seq of handlers.
   Each handler in the returned seq is a fnk from request to response.
   Conceptually, this function returns a curried version of the handlers
    that partially applies each handler to the resources."
  [handlers]
  (pfnk/fn->fnk
   (fn [resources] (map #(compile-handler resources %) handlers))
   [(->> handlers
         (map (comp :resources pfnk/input-schema))
         (reduce schema/union-input-schemata {}))
    [Handler]]))


(s/defn var->route-spec
  "If the input variable refers to a function that starts with a $,
    return the function annotated with the metadata of the variable."
  [var :- Var]
  (let [method-name (-> var meta (safe-get :name) name)]
    (when (and (.startsWith method-name "$") (fn? @var))
      (propagate-meta
       (propagate-meta (fn redefable [m] (@var m)) @var)
       var))))

(s/defn var->spec [var :- Var & [annotation-fn]]
  (let [method-name (-> var meta (safe-get :name) name)]
    (when (and (.startsWith method-name "$") (fn? @var))
      (let [spec nil



            handler
            (propagate-meta
             (propagate-meta (fn redefable [m] (@var m)) @var)
             var)]))))


(s/defn ns->handler-fns
  "Take a namespace and optional prefix, return a seq of all the functions that
    can be converted to handlers in the namespace annotated with metadata."
  ([ns-sym :- Symbol] (ns->handler-fns ns-sym ""))
  ([ns-sym :- Symbol route-prefix :- String]
     (assert (not (.startsWith route-prefix "$")))
     (assert (not (.endsWith route-prefix "$")))
     (->> (ns-interns ns-sym)
          vals
          (keep var->route-spec)
          (?>> (seq route-prefix)
               map #(vary-meta % assoc :route-prefix (str "$" route-prefix))))))

(s/defn nss->handlers-fn :- (s/=> [Handler] (s/named KeywordMap "resources"))
  "Take a map from prefix to namespace symbol,
   return a fnk from resources to a seq of fnk handlers,
    each of which takes a request and produces a response."
  [prefix->ns-sym :- {(s/named s/Str "route prefix")
                      (s/named Symbol "namespace")}]
  (->> prefix->ns-sym
       (mapcat (fn [[prefix ns-sym]] (ns->handler-fns ns-sym prefix)))
       compile-routes))
