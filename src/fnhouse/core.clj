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

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema Request
  (for-map [field [:uri-args :query-params :body]]
    (s/optional-key field) KeywordMap))

(s/defschema Handler
  (s/=> s/Any Request))

(s/defschema Symbol
  (s/pred symbol? 'symbol?))

(s/defschema Variable
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

(s/defn path [handler :- Handler]
  (get handler :path
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

(s/defn compile-routes :- (s/=> (s/named KeywordMap "resources") [Handler])
  "Take a seq of handlers and produce a single fnk from resources to seq of handlers.
   Each handler in the returned seq is a fnk from request to response.
   Conceptually, this function returns a curried version of the handlers
    that partially applies the handlers first to the resources and then to the request."
  [handlers]
  (pfnk/fn->fnk
   (fn [resources]
     (for [handler handlers]
       (propagate-meta
        (pfnk/fn->fnk
         (fn [request] (handler {:request request :resources resources}))
         [(:request (pfnk/input-schema handler) {})
          (pfnk/output-schema handler)])
        handler)))
   [(->> handlers
         (map (comp :resources pfnk/input-schema))
         (reduce schema/union-input-schemata {}))
    [Handler]]))

;; TODO use this to make handlers swankable
(defn var->route-fn [var]
  (fn [request] (@var request)))

(s/defn var->route-spec
  "If the input variable refers to a function that starts with a $,
    return the function annotated with the metadata of the variable."
  [var :- Variable]
  (let [method-name (-> var meta (safe-get :name) name)]
    (when (and (.startsWith method-name "$") (fn? @var))
      (propagate-meta @var var))))

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
          (map #(vary-meta % assoc-when :route-prefix
                           (when (seq route-prefix)
                             (str "$" route-prefix)))))))

(s/defn nss->handlers-fn :- (s/=> (s/named KeywordMap "resources") [Handler])
  "Take a map from prefix to namespace symbol,
   return a fnk from resources to a seq of fnk handlers,
    each of which takes a request and produces a response."
  [prefix->ns-sym :- {(s/named s/Str "route prefix")
                      (s/named Symbol "namespace")}]
  (->> prefix->ns-sym
       (mapcat (fn [[prefix ns-sym]] (ns->handler-fns ns-sym prefix)))
       compile-routes))
