(ns hayride.core-test
  (:use plumbing.core clojure.test hayride.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clj-http.client :as client]
   [fnhouse.handlers :as handlers]))

(def service-graph
  (graph/graph
   :counter (fnk [] (atom 0))))

(defnk $count$:arg$GET
  "Simple Counter"
  {:responses {200 {:body {:count s/Int}}}}
  [[:request [:uri-args arg :- Long]]
   [:resources counter]]
  {:body {:count (swap! counter inc)}})

(deftest start-api-test
  (let [handlers (handlers/nss->handlers-fn {"count" 'hayride.core-test})
        r (graph/run service-graph {})
        server (start-api {:handlers (handlers r)
                           :port 6054
                           :join? false})]
    (try
      (is (= {:count 1}
             (:body (client/get "http://localhost:6054/count/count/4"
                                {:as :json
                                 :request-method :get}))))

      (is (= {:count 2}
             (:body (client/get "http://localhost:6054/count/count/100"
                                {:as :json
                                 :request-method :get}))))
      (finally (.stop server)))))
