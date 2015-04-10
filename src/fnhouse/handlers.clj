(ns fnhouse.handlers
  "Utilities for turning a set of fnhouse handlers into an API description.

   A fnhouse handler is an ordinary Clojure function that accepts a map
   with two keys:

     :request is a Ring-style request [1] (see fnhouse.schemas/Request)
     :resources is an arbitrary map of resources (e.g., database handles).

   By default, the name of the function specifies the path and method of
   the handler (overridable with :path and :method metadata).  The handler
   must also be annotated with metadata describing schemas [2] for the required
   resources, key portions of the request (uri-args, query-params and body),
   and response bodies.

   The simplest way to specify this data is to use a `defnk` from
   plumbing.core [2], which can simultaneously destructure items from the
   resources and request, and produce the necessary corresponding schema
   annotations.

   For example, here is an example of a minimal fnhouse handler:

   (defnk unimaginative$GET
     {:responses {200 String}}
     []
     {:body \"Hello, world!\"})

   which defines a GET handler at path /unimaginative, which always returns
   the string \"Hello, world!\".

   A more complex example that illustrates most of the features of fnhouse:

   (s/defschema Idea {:name String :difficulty Double})

   (defn hammock$:id$ideas$POST
     \"Save a new idea to hammock :id, and return the list of existing ideas\"
     {:responses {200 [Idea]}}
     [[:request
       [:uri-args id :- Long]
       [:query-params {hard? :- Boolean false}]
       body :- Idea]
      [:resources ideas-atom]]
     {:body ((swap! ideas-atom update-in [id] conj
                    (if hard? (update-in idea [:difficulty] * 2) idea))
             id)})

   This is a handler that accepts POSTS at URIs like /hammock/12/ideas,
   with an optional Boolean query-param hard?, and a body that matches the
   Idea schema, adds the Idea to hammock 12, and returns the list of all
   current ideas at hammock 12.  The state of ideas is maintained in ideas-atom,
   which is explicitly passed in as a resource (assigned the default schema
   of s/Any by defnk).

   This handler can be called as an ordinary Clojure function (i.e., in tests),
   and runtime schema checking can be turned on following instructions in [2].

   The handler can also be turned into an API description by calling nss->handlers-fn
   (or related functions) and then passing in the map of resources, like:

   ((nss->handlers-fn {\"\" 'my-namespace})
    {:ideas-atom (atom {})})

   With this API description, you can do many things.  Out of the box, there is support
   for:
     - Turning the full API into a normal Ring handler using fnhouse.routes
     - Enabling schema checking and coercion using fnhouse.middleware
       (so, e.g., the Long id in uri-args is automatically parsed for you)
     - Producing minimal API docs
     - Generating model classes and client libraries for ClojureScript and
       Objective C using, e.g., coax [4]

   For a complete example, see the included 'examples/guesthouse' project.

   [1] https://github.com/ring-clojure
   [2] https://github.com/prismatic/schema
   [3] https://github.com/prismatic/plumbing
   [4] https://github.com/prismatic/coax"
  (:use plumbing.core)
  (:require
   [clojure.string :as str]
   [schema.core :as s]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as fnk-schema]
   [fnhouse.schemas :as schemas]
   [fnhouse.routes :as routes])
  (:import [clojure.lang Symbol Var]))

(def ^:dynamic ^String *path-separator*
  "The string to be used as a path separator in fnhouse fn names."
  "$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private schemas

(s/defschema Resources
  "A map of external resoures to be injected into a fnhouse handler"
  schemas/KeywordMap)

(s/defschema AnnotatedProtoHandler
  "A bundle of a raw fnhouse handler with its HandlerInfo"
  {:info schemas/HandlerInfo
   :proto-handler (s/=> schemas/Response
                        {:request schemas/Request
                         :resources Resources})})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extracting handler info from a fnhouse handler var

(defn ^:private ensure-leading-slash [^String s]
  (if (.startsWith s "/")
    s
    (str "/" s)))

(s/defn ^:private path-and-method
  "Extract the path and method from the var name, or :path & :method
   overrides in metadata.  (You must pass both overrides, or neither)."
  [var :- Var]
  (or (not-empty (select-keys (meta var) [:path :method]))
      (let [var-name (-> var meta (safe-get :name) name)
            last-idx (.lastIndexOf var-name *path-separator*)]
        {:path (-> var-name (subs 0 last-idx) (str/replace *path-separator* "/") ensure-leading-slash)
         :method (-> var-name (subs (inc last-idx)) str/lower-case keyword)})))

(s/defn ^:private source-map
  [var :- Var]
  (-> (meta var)
      (select-keys [:line :column :file :ns :name])
      (update-in [:name] name)
      (update-in [:ns] str)))

(defnk source-map->str [ns name file line]
  (format "%s/%s (%s:%s)" ns name file line))

(defn ^:private default-map-schema [schema]
  (if (= schema s/Any) {s/Keyword String} schema))

(s/defn var->handler-info :- schemas/HandlerInfo
  "Extract the handler info for the function referred to by the specified var."
  [var :- Var
   extra-info-fn]
  (letk [[method path] (path-and-method var)
         [{doc ""} {responses {}}] (meta var)
         [{resources {}} {request {}}] (pfnk/input-schema @var)
         [{uri-args s/Any} {query-params s/Any} {body nil} {custom nil}] request]
    (let [source-map (source-map var)
          explicit-uri-args (dissoc (default-map-schema uri-args) s/Keyword)
          raw-declared-args (routes/uri-arg-ks path)
          declared-args (set raw-declared-args)
          undeclared-args (remove declared-args (keys explicit-uri-args))
          info {:path path
                :method method
                :description doc
                :request {:query-params (default-map-schema query-params)
                          :body body
                          :uri-args (merge
                                     (map-from-keys (constantly String) declared-args)
                                     explicit-uri-args)
                          :custom custom}
                :responses responses

                :resources resources
                :source-map source-map
                :annotations (extra-info-fn var)}]
      (when-let [error (s/check schemas/HandlerInfo info)]
        (throw (IllegalArgumentException. (format "%s in %s" error (source-map->str source-map)))))
      (fnk-schema/assert-iae
       (empty? undeclared-args)
       "Undeclared args %s in %s" (vec undeclared-args) (source-map->str source-map))
      (fnk-schema/assert-iae
       (or (not (boolean body)) (boolean (#{:post :put :patch} method)))
       "Body only allowed in post or put method in %s" (source-map->str source-map))
      (fnk-schema/assert-iae
       (every? #{:resources :request s/Keyword} (keys (pfnk/input-schema @var)))
       "Disallowed non- :request or :resources bindings in %s: %s"
       (source-map->str source-map) (keys (pfnk/input-schema @var)))
      (fnk-schema/assert-iae
       (apply distinct? ::sentinel raw-declared-args) "Duplicate uri-args %s in %s"
       (vec raw-declared-args) (source-map->str source-map))
      info)))

(s/defn var->annotated-handler :- AnnotatedProtoHandler
  "Take a Var corresponding to a fnhouse handler and return an AnnotatedProtoHandler."
  [var :- Var
   extra-info-fn]
  {:info (var->handler-info var extra-info-fn)
   :proto-handler (pfnk/fn->fnk (fn redefable [m] (@var m))
                                (pfnk/io-schemata @var))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extracting ProtoHandlers and injecting resources to make them Handlers

(s/defn curry-resources :- (s/=> schemas/API Resources)
  "Take a sequence of AnnotatedProtoHandlers and return a function from resources
   to a set of normal AnnotatedHandlers with appropriate resources injected.
   Each handler only gets the specific top-level resources it asks for in its
   schema."
  [proto-handlers :- [AnnotatedProtoHandler]]
  (pfnk/fn->fnk
   (fn [all-resources]
     (for [proto-handler proto-handlers]
       (letk [[proto-handler info] proto-handler]
         (let [resources (select-keys all-resources (keys (:resources (pfnk/input-schema proto-handler))))]
           {:info info
            :handler (pfnk/fn->fnk
                      (fn [request] (proto-handler {:request request :resources resources}))
                      (update-in (pfnk/io-schemata proto-handler) [0] :request {}))}))))
   [(->> proto-handlers
         (map #(:resources (pfnk/input-schema (:proto-handler %)) {}))
         (reduce fnk-schema/union-input-schemata {}))
    schemas/API]))

(s/defn ^:private fnhouse-handler? [var :- Var]
  (and (fn? @var) (:responses (meta var))))

(s/defn apply-path-prefix :- schemas/HandlerInfo
  "Add a prefix to handler-info, which must consist of one or more complete path
   segments without URI args."
  [handler-info :- schemas/HandlerInfo prefix :- String]
  (fnk-schema/assert-iae
   (empty? (routes/uri-arg-ks prefix)) "Path prefix %s cannot contain uri args" prefix)
  (update-in handler-info [:path] (partial str (ensure-leading-slash prefix))))

(s/defn ns->handler-fns :- [AnnotatedProtoHandler]
  "Take a namespace, return a seq of all the AnnotatedProtoHandlers corresponding to
   fnhouse handlers in that namespace."
  [ns-sym :- Symbol
   extra-info-fn]
  (for [var (vals (ns-interns ns-sym))
        :when (fnhouse-handler? var)]
    (var->annotated-handler var extra-info-fn)))

(s/defn nss->proto-handlers :- [AnnotatedProtoHandler]
  "Take a map from path prefix string to namespace symbol.
   Sucks up all the fnhouse handlers in each namespace, and prefixes each handler's
   path with the corresponding path prefix. Finally, returns the resulting set of
   handlers."
  [prefix->ns-sym :- {(s/named String "path prefix")
                      (s/named Symbol "namespace")}
   & [extra-info-fn :- (s/maybe (s/=> s/Any Var))]]
  (->> prefix->ns-sym
       (mapcat (fn [[prefix ns-sym]]
                 (cond->> (ns->handler-fns ns-sym (or extra-info-fn (constantly nil)))
                          (seq prefix) (map (fn [annotated-handler]
                                              (update-in annotated-handler
                                                         [:info] apply-path-prefix prefix))))))))

(s/defn nss->handlers-fn :- (s/=> schemas/API Resources)
  "Partially build an API from a map of prefix string to namespace symbols.
   Returns a function that takes in the resources needed to construct the API,
   and gives a seq of AnnotatedHandlers with the resources partialed in."
  [prefix->ns-sym :- {(s/named String "path prefix")
                      (s/named Symbol "namespace")}
   & [extra-info-fn :- (s/=> s/Any Var)]]
  (-> prefix->ns-sym
      (nss->proto-handlers extra-info-fn)
      curry-resources))
