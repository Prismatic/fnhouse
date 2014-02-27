(ns fnhouse.routes
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [plumbing.map :as map]
   [fnhouse.handlers :as handlers]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; route-map-graphlike --> route-map-builder

(defn compile-route-graph
  "Take a nested map of fnks, and produce a fnk that produces an isomorphic map
   of the results of these fnks.  Unlike a graph, the fnks cannot depend on one-anothers outputs,
   and for now we're cutting corners on limiting keys, assuming fns aren't using :s or &."
  [handler-map]
  (pfnk/fn->fnk
   (fn [resources]
     (map-vals (fn [f] (f resources)) handler-map))
   [(->> handler-map
         vals
         (map pfnk/input-schema)
         (reduce schema/union-input-schemata {}))
    (map-vals pfnk/output-schema handler-map)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; namespace->route-map-graphlike

(defn propagate-meta [to from]
  ;; TODO: can we avoid this?
  (vary-meta to
             (fn [fnk-meta handler-meta]
               ;; I want the fnks meta to win out over the handler's meta
               (merge handler-meta fnk-meta))
             (meta from)))

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
      ;; A curried version of the handler that takes the resources and then the request
      {:method
       :path
       :declared-uri-args (set (keys (:uri-args (meta var))))
       :handler
       }
      [method-name
       (pfnk/fn->fnk
        (fn [resources]
          (propagate-meta
           (pfnk/fn->fnk
            (fn [request]
              (@var (assoc resources :request request)))
            [(:request (pfnk/input-schema @var) {})
             (pfnk/output-schema @var)])
           var))
        [(dissoc (pfnk/input-schema @var) :request)
         (s/=> (s/named java.util.Map "Ring Response") ;; TODO: pull out?
               (s/named java.util.Map "Ring Request"))])])))

;; TODO: leading or trailing slashes in prefix?!
(defn ns->route-graph
  "Take a ns, and produce a nested map of fnks that build requests as described in var-route-spec."
  ([ns-sym] (ns->route-graph ns-sym ""))
  ([ns-sym route-prefix]
     (->> (ns-interns ns-sym)
          vals
          (keep #(var->route-spec % middleware-override))
          (map-keys #(str route-prefix %)))))
