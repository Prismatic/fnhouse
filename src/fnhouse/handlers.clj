(ns fnhouse.handlers
  (:use plumbing.core)
  (:require
   [clojure.string :as str]
   [schema.core :as s])
  (:import [java.net URLDecoder]))

(s/defschema Method (s/enum :get :post :head :options :delete :put))

;; TODO: should we statically verify correspondence between :uri-args key
;; and the path and if so who should do this?

(s/defschema Route
  {:path String
   :method Method
   :handler (s/pred fn? 'fn?)
   (s/optional-key :declared-uri-args) #{:keyword}})


(defn url-decode
  "Returns an UTF-8 URL encoded version of the given string."
  [encoded]
  (URLDecoder/decode encoded "UTF-8"))


;; $docs$:doc-id$view
;; $docs$:arg$dwell

;; becomes:

;; {"/docs/*/view" {:uri-args [:doc-id] :f <function>}
;;  "/docs/*/dwell" {:uri-args [:arg] :f <function>}}


;;

(defn path->keyseq [^String path]
  (->> path
       handlers/split-path
       (mapcat (fn [^String s] (if (.startsWith s ":") [handlers/+wildcard+ (subs s 1)] [s])))
       (map keyword)
       (#(conj (vec %) +fn-key+))))

;; TODO: safe-unflatten

;; TODO: :* wildcards?
(deftype Wildcard [uri-arg]
  Object
  (hash-code [this] 0xdeadbeef)
  (equals [this x] (instance? Wildcard x)))

(let [a-wildcard (Wildcard. :whatever)]
  (defn find-wildcard [m]
    (when-let [[^Wildcard wc v] (find m a-wildcard)]
      [(.uri-arg wc) v])))

(def +wildcard+ ::*) ;; wildcard must map to map with single elt {:key etc}.


(defn wildcard-info [route-spec]
  "Extract [wildcard-key route-spec], or nil if no wildcard at this level"
  (when-let [m (+wildcard+ route-spec)]
    (assert (= 1 (count m)))
    (assert (keyword? (first (keys m))))
    (first m)))

(defn split-path [^String path]
  (->> (.split path "/")
       (remove #(.isEmpty ^String %))))

(defn default-head-to-get [route-spec request-method]
  (or (get route-spec request-method)
      (when-let [get-handler (and (= request-method :head)
                                  (get route-spec :get))]
        (fn-> (assoc :request-method :get) get-handler (assoc :body nil)))))

;;
(defn match-route-spec [route-spec uri-pieces request-method & [matched-uri matched-uri-skeleton uri-args]]
  (if (empty? uri-pieces)
    [(default-head-to-get route-spec request-method)
     (or matched-uri "/") (or matched-uri-skeleton "") (or uri-args {})]
    (let [matched-route (route-spec (keyword (first uri-pieces)))]
      (if-let [[wildcard-key new-route-spec] (if matched-route
                                               [nil matched-route]
                                               (wildcard-info route-spec))]
        (match-route-spec
         new-route-spec
         (rest uri-pieces)
         request-method
         (format "%s/%s" (or matched-uri "") (first uri-pieces))
         (format "%s/%s" (or matched-uri-skeleton "") (if matched-route (first uri-pieces) wildcard-key))
         (if matched-route uri-args (assoc uri-args wildcard-key (first uri-pieces))))

        ;; catchall matching, for backwards compatibility
        (when-let [h (default-head-to-get route-spec request-method)]
          [h (or matched-uri "/") (or matched-uri-skeleton "/") (or uri-args {});; for matching the root
           ])))))

(s/defn ^:always-validate compile-handler-spec [route-spec :- Object]
  (fn [{:keys [^String uri request-method] :as req}]
    (let [[h uri-prefix uri-skeleton uri-args]
          (match-route-spec route-spec (split-path uri) request-method)
          uri-args (map-vals url-decode uri-args)]
      (if h
        (h (assoc req
             :matched-uri uri-prefix
             :matched-uri-skeleton uri-skeleton
             :uri-args uri-args
             :uri-arg (.replaceFirst uri uri-prefix "")))
        {:status 404 :body "Not found"}))))
