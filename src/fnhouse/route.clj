(ns fnhouse.route
  (:use plumbing.core)
  (:require
   [clojure.string :as str]
   [schema.core :as s])
  (:import [java.net URLDecoder]))

(def single-wildcard ::*)

(def multiple-wildcard ::**)

(s/defn split-path :- [s/Str]
  [path :- String]
  (->> (.split path "/")
       (remove #(.isEmpty ^String %))))

(defn prefixes [x]
  (->> x
       (iterate rest)
       (take (count x))))

(defn all-splits [x]
  (map #(split-at % x) (range 1 (count x))))

(defn match-path [node path]
  (if-let [[x & xs] path]
    (or (match-path (get node x) xs)
        (match-path (get node :*) xs)
        (when-let [rec (get node :**)]
          (->> (prefixes xs)
               (keep #(match-path rec %))
               first)))
    node))

(defn match-path-x [node result path]
  (if-let [[x & xs] path]
    (or (match-path-x (get node x) (conj result x) xs)
        (match-path-x (get node :*) (conj result x) xs)
        (when-let [rec (get node :**)]
          (->> (all-splits path)
               (keep (fn [[match tail]]
                       (match-path-x rec (conj result match) tail)))
               first)))
    (when node
      {:match result
       :node node})))


(defn path->keyseq [^String path]
  (->> path
       split-path
       (mapcat (fn [^String s]
                 (if (.startsWith s ":")
                   [handlers/+wildcard+ (subs s 1)]
                   [s])))
       (map keyword)
       (#(conj (vec %) +fn-key+))))

(s/defn compile-handler [handler]

  )
