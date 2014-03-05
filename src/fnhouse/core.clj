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
  (:import [clojure.lang Namespace]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema RingRequest
  (map-keys
   s/optional-key
   {:uri-args KeywordMap
    :query-params KeywordMap
    :body s/Any}))

(s/defschema RingResponse
  s/Any)

(s/defschema Handler
  (s/=> RingResponse RingRequest))

(s/defschema Symbol
  (s/pred symbol? 'symbol?))

(s/defschema Var
  (s/pred var? 'var?))

(s/defschema Schema (s/protocol s/Schema))

(s/defschema Response
  {:description s/Str
   :value Schema})

(defrecord Responses [status->response-schema]
  s/Schema
  (walker [this]
    (let [status->subwalker
          (map-vals (fnk [value] (s/subschema-walker value)) status->response-schema)]
      (fnk [status :as x]
        (if-let [sub-walker (get status->subwalker status)]
          (sub-walker (dissoc x :status))
          (schema-macros/validation-error this x (list 'unschematized-status? (schema-utils/value-name x)))))))
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

   :responses Responses

   ;; maybe include a per-response doc format
   ;;:responses String ;;TODO: Responses

   :source-map (s/maybe
                {:line s/Int
                 :column s/Int
                 :file s/Str
                 :ns Namespace
                 :name Symbol})

   :annotations s/Any

   ;; Would you guys want this stuff? we can provide it!
   ;; :resources ??? ;; schema map, useful?
   ;; :file/line/var/ns ;; github link
   ;; :full-request-schema???? ;; probably not useful  (stuff other than params).
   ;; :full-response-schema???? ;; also probably not useful (stuff other than response body)
   })


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public

(defn responses [& args]
  (Responses.
   (for-map [[[code] body] (->> args (partition-by integer?) (partition 2))]
     code (case (count body)
            1 {:description "" :value (first body)}
            2 {:description (first body) :value (second body)}))))
