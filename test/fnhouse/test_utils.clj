(ns fnhouse.test
  (:use plumbing.core)
  (:require
   [clojure.test :as test]
   [clojure.data :as data]
   [clojure.pprint :as pprint]))

(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (pprint/pprint x#)
     x#))

(defn pprint-str [x]
  (with-out-str (pprint/pprint x)))

(defn pprint-diff [[left right both]]
  (pprint-str (array-map :left-side-only left
                         :right-side-only right
                         :both-sides both)))

(defn empty-diff? [[f s sh]] (and (nil? f) (nil? s)))

(defmacro is-=-by
  [key-fn x y]
  ;; first test/is is to catch exceptions in forms
  `(let [diff# (test/is (data/diff (~key-fn ~x) (~key-fn ~y)))]
     (when-not (empty-diff? diff#)
       (test/is (empty? [:the-diff])
                (pprint-diff diff#)))))

(defmacro is-= [x y]
  `(is-=-by identity ~x ~y))
