(ns fnhouse.schemas
  "Defines schemas for Handlers and HandlerInfo, fnhouse's API description format.
   See docstrings below for details."
  (:require
   [plumbing.fnk.schema :as fnk-schema]
   [schema.core :as s]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handlers

(s/defschema KeywordMap
  {s/Keyword s/Any})

(s/defschema Request
  "A Ring-style request, one input to a fnhouse handler"
  {:uri-args KeywordMap
   :query-params KeywordMap
   (s/optional-key :body) s/Any
   s/Any s/Any})

(s/defschema Response
  "A Ring-style response, the output of a fnhouse handler"
  {(s/optional-key :status) s/Int
   :body s/Any
   s/Any s/Any})

(s/defschema Handler
  (s/=> Response Request))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handler metadata

(s/defschema Schema
  "A meta-schema that matches Schemas"
  (s/protocol s/Schema))

(s/defschema HandlerInfo
  "A schema for information about a specific HTTP handler.

   path is the path to the handler, which can contain wildcards corresponding to uri-args;
   see fnhouse.routes for details.

   request describes schemas for the request, broken down into uri-args, query-params, and
   body (when applicable).

   responses is a mapping from status code to the response body schema for that status.

   resources is a schema for the resources that the handler requires in addition to the
   request (i.e., database handles).

   annotations is an arbitrary field that can be used to hold other user-defined fields;
   for example, authentication requirements or rate-limiting parameters.  It can be
   populated by passing an extra-info-fn to the functions in fnhouse.handlers."
  {;; HTTP-related info
   :path (s/both String (s/pred #(.startsWith ^String % "/") 'starts-with-slash?))
   :method (s/enum :get :head :post :put :delete :patch)

   :description String

   :request {:uri-args {s/Keyword Schema}
             :query-params Schema
             :body (s/maybe Schema)
             :custom (s/maybe Schema)}

   :responses {(s/named s/Int "status code")
               (s/named Schema "response body schema")}

   ;; Additional metadata about the Clojure implementation
   :resources fnk-schema/InputSchema

   :source-map (s/maybe
                {:line s/Int
                 :column s/Int
                 :file String
                 :ns String
                 :name String})

   :annotations (s/named s/Any "user-specified")})

(s/defschema AnnotatedHandler
  "A bundle of a resource-injected fnhouse handler with its HandlerInfo"
  {:info HandlerInfo
   :handler Handler})

(s/defschema API [AnnotatedHandler])
