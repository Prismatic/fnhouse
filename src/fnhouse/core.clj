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

(s/defschema ResponseCode s/Int)

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
