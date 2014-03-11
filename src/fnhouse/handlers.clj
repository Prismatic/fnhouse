(ns fnhouse.handlers
  (:use plumbing.core)
  (:require
   [clojure.string :as str]
   [schema.core :as s]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [fnhouse.core :as fnhouse])
  (:import [clojure.lang Symbol Var]))

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

(s/defn var-name :- String
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

(s/defn route-and-method [method-name :- String]
  (let [last-idx (.lastIndexOf method-name "$")]
    {:route (-> method-name (subs 0 last-idx) (str/replace "$" "/"))
     :method (-> method-name (subs (inc last-idx)) str/lower-case keyword)}))

(s/defn valid-handler?
  "Returns true for functions that can be converted into handlers"
  [var :- Var]
  (and (.startsWith (var-name var) "$") (fn? @var)))

(s/defn source-map [var :- Var]
  (-> (meta var)
      (select-keys [:line :column :file :ns :name])
      (update-in [:name] name)
      (update-in [:ns] str)))

(defnk source-map->str [ns name file line]
  (format "%s/%s (%s:%s)" ns name file line))

(s/defn var->info :- fnhouse/HandlerInfo
  "Extract the handler info for the function referred to by the specified var."
  [route-prefix :- s/Str
   var :- Var
   extra-info-fn]
  (letk [[method route] (-> var var-name route-and-method)
         [{doc ""} {path route} {responses {}}] (meta var)
         [{resources {}} {request {}}] (pfnk/input-schema @var)
         [{uri-args {}} {body nil} {query-params {}}] request]
    (let [source-map (source-map var)
          explicit-uri-args (dissoc uri-args s/Keyword)
          full-path (str route-prefix path)
          declared-args (declared-uri-args full-path)
          undeclared-args (remove declared-args (keys explicit-uri-args))
          info {:request {:query-params query-params
                          :body body
                          :uri-args (merge
                                     (map-from-keys (constantly s/Str) declared-args)
                                     explicit-uri-args)}

                :path full-path
                :method method

                :responses responses

                :resources resources
                :short-description (-> doc (str/split #"\n" 2) first)
                :description doc

                :source-map source-map
                :annotations (extra-info-fn var)}]

      (when-let [error (s/check fnhouse/HandlerInfo info)]
        (throw (IllegalArgumentException. (format "%s in %s" error (source-map->str source-map)))))
      (assert
       (empty? undeclared-args)
       (format "Undeclared args %s in %s" (vec undeclared-args) (source-map->str source-map)))
      (assert
       (or (not (boolean body)) (boolean (#{:post :put} method)))
       (str "Body only allowed in post or put method in " (source-map->str source-map)))
      info)))

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
  (let [route-prefix (if (seq route-prefix) (str "/" route-prefix) "")]
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
