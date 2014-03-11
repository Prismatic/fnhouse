(ns hayride.domain
  (:require [schema.core :as s])
  )

(s/defschema Task
  {:user String
   :task String})
