(ns hayride.core-test
  (:use plumbing.core clojure.test hayride.core)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]
   [clj-http.client :as client]
   [fnhouse.handlers :as handlers]))

(def service-graph
  (graph/graph
   :counter (fnk [] (atom 0))
   :messages (fnk [] (atom []))))

(defnk $count$:arg$GET
  "Simple Counter"
  {:responses {200 {:body {:arg Long
                           :count s/Int}}}}
  [[:request [:uri-args arg :- Long]
    [:query-params qp1 :- s/Any]]
   [:resources counter]]
  {:body {:arg arg
          :count (swap! counter inc)}})


(defnk $mail-drop$POST
  "Simple Counter"
  {:responses {200 {:body {:ack String}}}}
  [[:request body :- {:to s/Keyword
                      :from s/Keyword
                      :msg String}]
   [:resources messages]]
  (swap! messages conj body)
  {:body {:ack "true"}})

(deftest start-api-test
  (let [handlers (handlers/nss->handlers-fn {"count" 'hayride.core-test})
        r (graph/run service-graph {})
        server (start-api {:handlers (handlers r)
                           :port 6054
                           :join? false})]
    (try
      (is (= {:count 1 :arg 1}
             (:body (client/get "http://localhost:6054/count/count/1"
                                {:as :json
                                 :request-method :get
                                 :query-params {:qp1 "hi"}
                                 }))))

      (is (= {:count 2 :arg 2}
             (:body (client/get "http://localhost:6054/count/count/2"
                                {:as :json
                                 :request-method :get
                                 :query-params {:qp1 "hi"}
                                 }))))


      (is (= {:ack "true"}
             (:body (client/post "http://localhost:6054/count/mail-drop"
                                 {:as :json
                                  :content-type :json
                                  :body (cheshire.core/generate-string {:to :you
                                                                        :from :me
                                                                        :msg "hello!"})}))))
      (is (= [{:to :you
               :from :me
               :msg "hello!"}]
             @(:messages r)))
      (finally (.stop server)))))
