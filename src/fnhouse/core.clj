(ns fnhouse.core
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clojure.string :as str]
   [clojure.pprint :as pprint]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [plumbing.map :as map]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessor Helpers

(defn- handler-input [handler-var input]
  (safe-get-in (pfnk/input-schema @handler-var) [:request input]))

(defn- handler-property [handler-var property]
  (safe-get (meta handler-var) property))

#_(defn path [handler-var]
    (-> (handler-property handler-var :name)
        (str/split #"\$")
        (map #(if (.startsWith)) (Char-))
        ))

(defn uri-args [handler-var]
  (handler-input handler-var :uri-args))

(defn query-params [handler-var]
  (handler-input handler-var :query-params))

(defn body [handler-var]
  (handler-input handler-var :body))

(defn private? [handler-var]
  (boolean (get (meta handler-var) :private)))

(defn description [handler-var]
  (handler-property handler-var :doc))

(defn short-description [handler-var]
  (-> (handler-property handler-var :doc)
      (str/split #"\n" 2)
      first))

(defn responses [handler-var]
  (pfnk/output-schema @handler-var))


(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (pprint/pprint x#)
     x#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; route-map-graphlike --> route-map-builder

(defn propagate-meta [to from]
  ;; TODO: can we avoid this?
  (vary-meta to
             (fn [fnk-meta handler-meta]
               ;; I want the fnks meta to win out over the handler's meta
               (merge handler-meta fnk-meta))
             (meta from)))


;; A curried version of the handler that takes the resources and then the request
(defn compile-routes
  "Take a map of handler->fnk, and produce a single fnk that produces an isomorphic map
   of the results of these fnks.  Unlike a graph, the fnks cannot depend on one-anothers outputs,
   and for now we're cutting corners on limiting keys, assuming fns aren't using :s or &."
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
    [s/Any]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; namespace->route-map-graphlike


;; TO make it swankable
(defn var->route-fn [var]
  (fn [request] (@var request)))

(s/defn var->route-spec
  "Take a var as input.  If the var points at a fnk and has :methods in the metadata,
   produce a [path-string route-maker] pair where route-maker is a fnk that takes any
   resources and returns a route fn.  The original var will be swankable, so long as new
   resource arguments are not added.

   The path is computed taking :path string in metadata or replacing $ in the var name with /.
   If the metadata has :middleware (also a fnk), the returning builder applies the metadata
   produced by the middleware fn the the route fn."
  [var :- (s/pred var? 'var?)]
  ;; TODO: allow path override
  (let [method-name (name (safe-get (meta var) :name))]
    (when (and (.startsWith method-name "$") (fn? @var))
      @var)))

;; TODO: leading or trailing slashes in prefix?!
(defn ns->route-graph
  "Take a ns, and produce a nested map of fnks that build requests as described in var-route-spec."
  ([ns-sym] (ns->route-graph ns-sym ""))
  ([ns-sym route-prefix]
     (->> (ns-interns ns-sym)
          vals
          (keep var->route-spec)
          (map #(vary-meta % assoc :route-prefix route-prefix)))))

(defn nss->handlers-fn [ns-syms-and-prefixes] ;; map?
  (->> ns-syms-and-prefixes
       (mapcat (fn [[prefix namespace]] (ns->route-graph namespace prefix)))
       compile-routes))

#_(nss->handlers-fn
   {"news" 'api.v2.news})
