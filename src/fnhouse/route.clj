(ns fnhouse.route
  (:use plumbing.core)
  (:require
   [plumbing.map :as map]
   [fnhouse.core :as fnhouse]
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

(def +handler-key+
  "Used to ensure disjoint-prefix property of route map:
    all handlers are at leaf nodes"
  ::handler)

(s/defschema MatchResult
  [(s/either
    s/Keyword
    (s/named [s/Keyword] "multiple-wildcard match"))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private

(s/defn split-path :- [s/Str]
  [path :- s/Str]
  (->> (.split path "/")
       (keep (fn [^String segment]
               (when-not (.isEmpty segment)
                 segment)))))

(defn all-splits [x]
  (map #(split-at % x) (range 1 (count x))))

(s/defn prefix-lookup
  "Recursively looks up the specified path starting at the given node.
   If there is a handler located at the specified path,
    returns the handler and the matching path segments, grouping the multiple segments
    that match a multiple-wildcard into a single seq.
    Multiple-wildcards are greedy: if there is ambiguity,
     earlier wildcards capture more than the ones that follow them.

   At each level, the lookup prioritizes literal matches over single-wildcards,
    and single-wildcards over multiple-wildcards.
    The search will backtrack to try all possible matching routes.
    Returns nil if no match is found."
  ([node path] (prefix-lookup node [] (conj path +handler-key+)))
  ([node
    result :- MatchResult
    path :- [s/Keyword]]
     (let [[x & xs] path]
       (if (= x +handler-key+)
         (when-let [handler (get node +handler-key+)]
           {:match-result result
            :handler handler})
         (or
          (prefix-lookup (get node x) (conj result x) xs)
          (prefix-lookup (get node +single-wildcard+) (conj result x) xs)
          (when-let [rec (get node +multiple-wildcard+)]
            (->> (all-splits path)
                 (keep (fn [[match tail]]
                         (prefix-lookup rec (conj result match) tail)))
                 first)))))))


(s/defn uri-arg [s :- String]
  (when (.startsWith s ":")
    (keyword (subs s 1))))

(s/defn match-segment :- s/Keyword
  [segment :- s/Str]
  (cond
   (uri-arg segment) +single-wildcard+
   (= "*" segment) +multiple-wildcard+
   :else (keyword segment)))

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
  (->> (str/upper-case (name request-method))
       (format "%s/%s" uri)
       split-path
       (map keyword)
       vec))

(s/defn compile-handler [handler]
  (let [split (-> handler fnhouse/path (.replaceAll "\\$" "/") split-path)]
    {:handler handler
     :match-path (-> (map match-segment split) vec (conj +handler-key+))
     :uri-arg-map (uri-arg-map split)}))

(defn build-prefix-map [handlers]
  (->> handlers
       (map (comp (juxt :match-path identity) compile-handler))
       map/unflatten))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn root-handler
  "Takes a seq of handlers, returns a single handler that routes the request, and processes
    uri arguments to the appropriate input handler based on the path."
  [handlers]
  (let [prefix-map (build-prefix-map handlers)]
    (fn [request]
      (if-let [found (->> request request->path-seq (prefix-lookup prefix-map))]
        (letk [[match-result [:handler handler uri-arg-map]] found]
          (->> match-result
               (realize-uri-args uri-arg-map)
               (assoc request :uri-args)
               handler))
        {:status 404 :body "Not found."}))))
