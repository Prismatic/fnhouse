(ns fnhouse.schemas
  (:use plumbing.core)
  (:require [schema.core :as s]))

;; maybe replace this with a more conventional thing.. look at pedestal
(s/defschema Handler
  (for-map [method [:post :get :delete :head]]
    (s/optional-key method) s/Any))

(s/defschema Routes
  (assoc Handler s/Keyword (s/recursive #'Routes)))

;; possibly look at:  api.v2.domain.responses
(s/defschema HandlerMeta
  {(s/optional-key :uri-args) {s/Keyword s/Any}
   (s/optional-key :query-params) {s/Keyword s/Any}
   :description String
   :returns {(s/named s/Int "status-code") {s/Keyword s/Any}}

   ;; maybe remove..
   :auth #{:post-auth :public :admin}})
