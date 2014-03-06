(ns fnhouse.routes
  (:use plumbing.core)
  (:require
   [plumbing.map :as map]
   [fnhouse.core :as fnhouse]
   [fnhouse.handlers :as handlers]
   [clojure.string :as str]
   [schema.core :as s])
  (:import [java.net URLDecoder]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Definitions

(def +single-wildcard+
  "Matches a single route segment"
  ::*)

(def +multiple-wildcard+
  "Matches one or more route segments"
  ::**)

(s/defschema MatchResult
  [(s/either
    s/Keyword
    (s/named [s/Keyword] "multiple-wildcard match"))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(defn safe-unflatten
  "Transform a seq of [keyseq leaf-val] pairs into a nested map.
   If one keyseq is a prefix of another, you're on your own."
  [s]
  (reduce (fn [m [ks v]]
            (if (seq ks)
              (update-in
               m ks
               (fn [old-value]
                 (assert (nil? old-value) (str "duplicate keyseq " ks))
                 v))
              v))
          {}
          s))

(s/defn prefix-lookup
  "Recursively looks up the specified path starting at the given node.
   If there is a handler located at the specified path,
    returns the handler and the matching path segments, grouping the multiple segments
    that match a multiple-wildcard into a single seq.
    Multiple-wildcards can only appear above the leaf.

   At each level, the lookup prioritizes literal matches over single-wildcards,
    and single-wildcards over multiple-wildcards.
    The search will backtrack to try all possible matching routes.
    Returns nil if no match is found."
  ([node path] (prefix-lookup node [] path))
  ([node result :- MatchResult path :- [(s/either s/Str s/Keyword)]]
     (let [[x & xs] path]
       (if (keyword? x)
         (when-let [handler (get node x)]
           {:match-result result
            :handler handler})
         (or
          (first
           (keep
            (fn [lookup-key]
              (when-let [subtree (get node lookup-key)]
                (prefix-lookup subtree (conj result x) xs)))
            [x +single-wildcard+]))
          (when-let [rec (get node +multiple-wildcard+)]
            (let [[match remainder] (split-with (comp not keyword?) path)]
              (prefix-lookup rec (conj result match) remainder))))))))

(s/defn uri-arg [s :- String]
  (when (.startsWith s ":")
    (keyword (subs s 1))))

(s/defn match-segment :- s/Keyword
  [segment :- s/Str]
  (cond
   (= ":*" segment) +multiple-wildcard+
   (uri-arg segment) +single-wildcard+
   :else segment))

(defn uri-arg-map [split]
  (->> split
       (keep-indexed
        (fn [i segment]
          (when-let [arg (uri-arg segment)]
            [i arg])))
       (into {})))

(defn realize-uri-args [uri-arg-map match-result]
  (for-map [[idx uri-arg] uri-arg-map]
    uri-arg (get match-result idx)))

(defnk request->path-seq [uri request-method :as request]
  (-> (vec (handlers/split-path uri))
      (conj request-method)
      vec))

(defnk compile-handler [handler [:info path method]]
  (let [split (handlers/split-path path)]
    {:handler handler
     :match-path (conj (vec (map match-segment split)) method)
     :uri-arg-map (uri-arg-map split)}))

(defn build-prefix-map [handlers]
  (->> handlers
       (map (comp (juxt :match-path identity) compile-handler))
       map/unflatten))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(s/defn root-handler
  "Takes a seq of handlers, returns a single handler that routes the request, and processes
    uri arguments to the appropriate input handler based on the path."
  [handlers :- [handlers/AnnotatedHandler]]
  (let [prefix-map (build-prefix-map handlers)]
    (fn [request]
      (if-let [found (->> request request->path-seq (prefix-lookup prefix-map))]
        (letk [[match-result [:handler handler uri-arg-map]] found]
          (->> match-result
               (realize-uri-args uri-arg-map)
               (assoc request :uri-args)
               handler))
        {:status 404 :body "Not found."}))))
