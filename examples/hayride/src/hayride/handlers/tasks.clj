(ns hayride.handlers.tasks
  (:require
   [hayride.domain :as domain]))


;; TODO: maybe make paginated?
(defnk $GET
  {:description "List all the tasks"
   :returns [domain/Task]}
  [tasks]
  {:body @tasks})

(defnk $POST
  {:description "Post a task"
   :body Task
   :returns {200 String}}
  [tasks [:request body]]
  (swap! tasks conj body)
  {:body "ACK"})
