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
  (:import [clojure.lang Namespace Symbol Var]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema RingRequest
  (map-keys
   s/optional-key
   {:uri-args KeywordMap
    :query-params KeywordMap
    :body s/Any}))

(s/defschema RingResponse
  {(s/optional-key :status) s/Int
   (s/optional-key :headers) s/Any
   :body s/Any})

(s/defschema RingHandler
  (s/=> RingResponse RingRequest))

(s/defschema Resources
  KeywordMap)

(s/defschema AnnotatedHandler
  {:info fnhouse/HandlerInfo
   :handler RingHandler})

(s/defschema ProtoHandler
  (s/=> RingResponse
        {:request RingRequest
         :resources Resources}))

(s/defschema AnnotatedProtoHandler
  {:info fnhouse/HandlerInfo
   :proto-handler ProtoHandler})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(s/defn function-name :- String
  [var :- Var]
  (-> var meta (safe-get :name) name))

(s/defn split-path :- [s/Str]
  [path :- String]
  (keep not-empty (str/split path #"/")))

(s/defn uri-arg :- (s/maybe s/Keyword)
  [s :- String]
  (when (.startsWith s ":")
    (keyword (subs s 1))))

(s/defn declared-uri-args :- #{s/Keyword}
  "Returns the set of uri-args present in the method path"
  [full-path :- s/Str]
  (->> full-path split-path (keep uri-arg) set))

(s/defn parse-method-name [method-name :- String] ;; TODO name is confusing
  (let [last-idx (.lastIndexOf method-name "$")]
    {:route (-> method-name (subs 0 last-idx)) ;; TODO Also munge?
     :method (-> method-name (subs (inc last-idx)) str/lower-case keyword)}))


(s/defn valid-handler?
  "Returns true for functions that can be converted into handlers"
  [var :- Var]
  (and (.startsWith (function-name var) "$") (fn? @var)))

;; TODO: inline these
(defn validate-uri-args [source-map uri-args declared-args]
  (let [undeclared-args (remove declared-args (keys uri-args))]
    (assert
     (empty? undeclared-args)
     (format "Undeclared args %s in %s" (vec undeclared-args) source-map))))

(defn validate-body [source-map method body]
  (assert
   (or (not (boolean body)) (boolean (#{:post :put} method)))
   (str "Body only allowed in post or put method in " source-map)))

(s/defn ^:always-validate var->info :- fnhouse/HandlerInfo
  "Extract the handler info for the function referred to by the specified var."
  [route-prefix :- s/Str
   var :- Var
   extra-info-fn]
  (letk [[method route] (-> var function-name parse-method-name)
         [{doc ""} {path route}] (meta var)
         [{resources {}} {request {}}] (pfnk/input-schema @var)
         [{uri-args {}} {body nil} {query-params {}}] request]
    (let [explicit-uri-args (dissoc uri-args s/Keyword)
          full-path (str/replace (str route-prefix path) "$" "/")
          declared-args (declared-uri-args full-path)
          source-map (select-keys (meta var) [:line :column :file :ns :name])]
      (validate-uri-args source-map explicit-uri-args declared-args)
      (validate-body source-map method body)
      {:request {:query-params query-params
                 :body (dissoc body s/Keyword)
                 :uri-args (merge
                            (map-from-keys (constantly s/Str) declared-args)
                            uri-args)}

       :path full-path
       :method method

       :responses (pfnk/output-schema @var)

       :resources resources
       :short-description (-> doc (str/split #"\n" 2) first)
       :description doc

       :source-map source-map
       :annotations (extra-info-fn var)})))

(s/defn compile-handler :- AnnotatedHandler
  "Partially apply the the handler to the resources"
  [resources handler :- AnnotatedProtoHandler]
  (letk [[proto-handler info] handler]
    {:info info
     :handler (pfnk/fn->fnk
               (fn [request] (proto-handler {:request request :resources resources}))
               (update-in (pfnk/io-schemata proto-handler) [0] :request {}))}))

(s/defn curry-handlers :- (s/=> [AnnotatedHandler] Resources)
  "Compute a curried version of the handlers that partially
    applies each proto-handler to the resources."
  [proto-handlers :- [AnnotatedProtoHandler]]
  (pfnk/fn->fnk
   (fn [resources] (map #(compile-handler resources %) proto-handlers))
   [(->> proto-handlers
         (map #(:resources (pfnk/input-schema (:proto-handler %)) {}))
         (reduce schema/union-input-schemata {}))
    [AnnotatedHandler]]))

(s/defn ns->handler-fns :- [AnnotatedProtoHandler]
  "Take a route prefix and namespace, return a seq of all the functions
    can be converted to handlers in the namespace along with their handler info."
  [route-prefix :- String
   ns-sym :- Symbol
   extra-info-fn]
  (assert (not (.startsWith route-prefix "$")))
  (assert (not (.endsWith route-prefix "$")))
  (let [route-prefix (if (seq route-prefix) (str "$" route-prefix) "")]
    (for [var (vals (ns-interns ns-sym))
          :when (valid-handler? var)]
      {:info (var->info route-prefix var extra-info-fn)
       :proto-handler (pfnk/fn->fnk (fn redefable [m] (@var m))
                                    (pfnk/io-schemata @var))})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(s/defn nss->handlers-fn :- (s/=> [AnnotatedHandler] Resources)
  [prefix->ns-sym :- {(s/named s/Str "route prefix")
                      (s/named Symbol "namespace")}
   & [extra-info-fn :- (s/=> s/Any Var)]]
  (->> prefix->ns-sym
       (mapcat (fn [[prefix ns-sym]] (ns->handler-fns prefix ns-sym (or extra-info-fn (constantly nil)))))
       curry-handlers))

(set! *warn-on-reflection* false)
