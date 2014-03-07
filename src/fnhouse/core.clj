(ns fnhouse.core
  "Utilities and helpers for converting namespaces of functions into handlers."
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clojure.string :as str]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [schema.macros :as schema-macros]
   [schema.utils :as schema-utils]
   [plumbing.map :as map])
  (:import [clojure.lang Namespace Symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema

(s/defschema Schema (s/protocol s/Schema))

(s/defschema Response
  {:description s/Str
   :value Schema})

(s/defschema ResponseCode s/Int)

(defrecord ResponseBodies [status->response-schema]
  s/Schema
  (walker [this]
    (let [status->subwalker
          (map-vals (fnk [value] (s/subschema-walker value)) status->response-schema)]
      (fnk [{status 200} :as response]
        (if-let [sub-walker (get status->subwalker status)]
          (sub-walker (:body response))
          (->> (list 'unschematized-status? (schema-utils/value-name response))
               (schema-macros/validation-error this response))))))
  (explain [this] nil
    (list 'responses status->response-schema)))

(s/defschema HandlerInfo
  {:path String
   :method (s/enum :get :head :post :put :delete)

   :short-description s/Str
   :description s/Str

   :request {:body (s/maybe Schema)
             :query-params schema/InputSchema
             :uri-args {s/Keyword Schema}}

   :resources schema/InputSchema

   :responses {ResponseCode Schema}

   :source-map (s/maybe
                {:line s/Int
                 :column s/Int
                 :file s/Str
                 :ns s/Str
                 :name s/Str})

   :annotations s/Any

   ;; Would you guys want this stuff? we can provide it!
   ;; :full-request-schema???? ;; probably not useful  (stuff other than params).
   ;; :full-response-schema???? ;; also probably not useful (stuff other than response body)
   })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

;; maybe in other file.

(defmacro spy [x & [context]]
  `(let [context# ~context
         x# ~x
         file# ~*ns*
         line# ~(:line (meta &form))]
     (println "SPY" (str file# ":" line# " " context#))
     (clojure.pprint/pprint x#)
     x#))


(defn responses [& args]
  (throw (Exception. "e"))
  (assert (integer? (first args)) "arguments must specify a status response code")
  (ResponseBodies.
   (for-map [[[code] body] (->> args (partition-by integer?) (partition-all 2))]
     code (case (count body)
            1 {:description "" :value (s/validate Schema (first body))}
            2 {:description (s/validate String (first body))
               :value (s/validate Schema (second body))}
            (throw (Exception.
                    (str args " is not a sequence of: <response-code> "
                         "<optional-description> <body-schema>")))))))
