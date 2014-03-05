(ns fnhouse.handlers
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [fnhouse.core :as fnhouse]
   [clojure.string :as str]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [plumbing.map :as map])
  (:import [clojure.lang Namespace]))

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema RingRequest
  (map-keys
   s/optional-key
   {:uri-args KeywordMap
    :query-params KeywordMap
    :body s/Any}))

(s/defschema Schema (s/protocol s/Schema))

(s/defschema RingResponse
  s/Any)

(s/defschema RingHandler
  (s/=> RingResponse RingRequest))

(s/defschema Handler
  {:handler-info fnhouse/HandlerInfo
   :handler RingHandler})

(s/defschema Symbol
  (s/pred symbol? 'symbol?))

(s/defschema Var
  (s/pred var? 'var?))

(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (clojure.pprint/pprint x#)
     x#))

(s/defn split-path :- [s/Str]
  [path :- s/Str]
  (keep not-empty (.split path "\\$")))

(s/defn uri-arg [s :- String]
  (when (.startsWith s ":")
    (keyword (subs s 1))))

(s/defn parse-handler-name [handler-name :- s/Str]
  (let [last-idx (.lastIndexOf handler-name "$")]
    {:route (-> handler-name
                (subs 0 last-idx))
     :method (-> handler-name
                 (subs (inc last-idx))
                 str/lower-case
                 keyword)}))

(defn validate-uri-args [source-map uri-args declared-args]
  (let [undeclared-args (remove
                         (some-fn (comp not keyword?) declared-args)
                         (keys uri-args))]
    (assert
     (empty? undeclared-args)
     (format "Undeclared args %s in %s" (vec undeclared-args) source-map))))

(defn validate-body [source-map method body]
  (assert
   (= (boolean (#{:post :put} method)) (boolean body))
   (str "Body present in non-post or put method in " source-map)))

(s/defn method-name :- String
  [var :- Var]
  (-> var meta (safe-get :name) name))

(s/defn valid-handler? [var :- Var]
  (and (.startsWith (method-name var) "$") (fn? @var)))

(s/defn declared-uri-args :- #{s/Keyword}
  [full-path :- s/Str]
  (->> full-path split-path (keep uri-arg) set))

(s/defn ^:always-validate var->handler-info :- fnhouse/HandlerInfo
  "If the input variable refers to a function that starts with a $,
    return the function annotated with the metadata of the variable."
  [route-prefix :- s/Str
   var :- Var
   & [annotation-fn]]
  (letk [[method route] (-> var method-name parse-handler-name)
         [{doc ""} {path route}] (meta var)
         [{resources {}} [:request {uri-args {}} {body nil} {query-params {}}]] (pfnk/input-schema @var)]
    (let [full-path (str route-prefix path)
          declared-args (declared-uri-args full-path)
          source-map (select-keys (meta var) [:line :column :file :ns :name])]
      (validate-uri-args source-map uri-args declared-args)
      (validate-body source-map method body)
      {:request {:query-params query-params
                 :body (dissoc body s/Keyword)
                 :uri-args (merge
                            (map-from-keys (constantly s/Str) declared-args)
                            (dissoc uri-args s/Keyword))}

       :path full-path
       :method method

       ;; TODO :responses

       :resources resources
       :short-description (-> doc (str/split #"\n" 2) first)
       :description doc

       :source-map source-map
       :annotations (when annotation-fn (annotation-fn var))})))

(defn propagate-meta
  "Copy the meta from one object into another without overwriting any existing fields."
  [to from]
  (vary-meta
   to
   (fn [to-meta from-meta] (merge from-meta to-meta))
   (meta from)))

(s/defn compile-handler :- Handler
  "Partially apply the the handler to the resources and return a fnk from request to response"
  [resources handler]
  (letk [[proto-handler handler-info] handler]
    (-> handler
        (dissoc :proto-handler)
        (assoc :handler
          (pfnk/fn->fnk
           (fn [request] (proto-handler {:request request :resources resources}))
           [(:request handler-info) (:response handler-info)])))))

(s/defn compile-routes
  "Take a seq of handlers and produce a single fnk from resources to seq of handlers.
   Each handler in the returned seq is a fnk from request to response.
   Conceptually, this function returns a curried version of the handlers
    that partially applies each handler to the resources."
  [handlers]
  (pfnk/fn->fnk
   (fn [resources] (map #(compile-handler resources %) handlers))
   [(->> handlers
         (map #(safe-get-in % [:handler-info :resources]))
         (reduce schema/union-input-schemata {}))
    [Handler]]))

(s/defn ns->handler-fns
  "Take a namespace and optional prefix, return a seq of all the functions that
    can be converted to handlers in the namespace annotated with metadata."
  [route-prefix :- String
   ns-sym :- Symbol
   & [annotation-fn]]
  (assert (not (.startsWith route-prefix "$")))
  (assert (not (.endsWith route-prefix "$")))
  (let [route-prefix (if (seq route-prefix) (str "$" route-prefix) "")]
    (for [var (vals (ns-interns ns-sym))
          :when (valid-handler? var)]
      {:handler-info (var->handler-info route-prefix var annotation-fn)
       :proto-handler (-> (fn redefable [m] (@var m))
                          (propagate-meta @var)
                          (propagate-meta var))})))

(s/defn nss->handlers-fn :- (=> [Handler] (s/named KeywordMap "resources"))
  "Take a map from prefix to namespace symbol,
   return a fnk from resources to a seq of fnk handlers,
    each of which takes a request and produces a response."
  [prefix->ns-sym :- {(s/named s/Str "route prefix")
                      (s/named Symbol "namespace")}]
  (->> prefix->ns-sym
       (mapcat #(apply ns->handler-fns %))
       compile-routes))
