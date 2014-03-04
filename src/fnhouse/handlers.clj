(ns fnhouse.handlers
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clojure.string :as str]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema]
   [plumbing.map :as map]))

(s/defschema Schema (s/protocol s/Schema))

(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (clojure.pprint/pprint x#)
     x#))


(s/defn parse-handler-name [handler-name :- s/Str]
  (let [last-idx (.lastIndexOf handler-name "$")]
    {:route (-> handler-name
                (subs 0 last-idx)
                (str/replace "$" "/"))
     :method (-> handler-name
                 (subs (inc last-idx))
                 str/lower-case
                 keyword)}))

(defnk $test$handler$:uri-arg$GET :- {:success? Boolean}
  "This is my test handler.

   It depends on resources and a request, and produces a result."
  {:route-prefix "$XXX"}
  [[:request
    [:body body-arg :- s/Keyword]
    [:uri-args uri-arg :- s/Int]
    [:query-params qp1 :- s/Str {qp2 :- s/Int 3}]]
   [:resources data-store]]
  (swap! data-store
         conj
         {:body-arg body-arg
          :uri-arg uri-arg
          :qp1 qp1
          :qp2 qp2})
  {:body {:success? true}})


(defn var->spec [var & [annotation-fn]]
  (letk [[method route] (parse-handler-name (-> var meta :name name))
         [doc {path route} {route-prefix nil}] (meta var)
         [[:request {uri-args {}} {body nil}
           {query-params {s/Keyword Schema}}]] (pfnk/input-schema @var)]
    #_    (validate-uri-args uri-args path)
    {:uri-args uri-args
     :body (dissoc body s/Keyword)
     :path (str route-prefix path)
     :method method

     #_ (;; TODO
         :responses
         )

     :short-description (-> doc (str/split #"\n" 2) first)
     :description doc

     :query-params query-params
     :source-map (select-keys (meta var) [:line :column :file :ns :name])
     :annotations (when annotation-fn (annotation-fn var))}))

(println (meta  #'$test$handler$:uri-arg))

(clojure.pprint/pprint (fnhouse.handlers/var->spec #'$test$handler$:uri-arg$GET))
