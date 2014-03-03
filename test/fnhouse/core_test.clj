(ns fnhouse.core-test
  (:use clojure.test plumbing.core fnhouse.core)
  (:require
   [schema.core :as s]))

(defnk $test$handler$:uri-arg :- {:success? Boolean}
  "This is my test handler.

   It depends on resources and a request, and produces a result."
  [[:request
    [:body body-arg :- s/Keyword]
    [:uri-args uri-arg :- s/Int]
    [:query-params qp1 :- s/Str {qp2 :- s/Int 3}]]
   [:resources data-store]]
  (swap! data-store
         conj
         {:body-arg body-arg
          :uri-arg uri-arg
          :qp1 qp1
          :qp2 qp2})
  {:body {:success? true}})

(deftest core-test
  (let [;; example resource
        data-store (atom [])

        svc-fn (nss->handlers-fn {"test" 'fnhouse.core-test})
        service (svc-fn {:data-store data-store})
        handler (singleton service)]

    (is (= {s/Keyword s/Any :uri-arg s/Int} (uri-args handler)))
    (is (= {s/Keyword s/Any
            :qp1 s/Str
            (s/optional-key :qp2) s/Int}
           (query-params handler)))
    (is (not (private? handler)))

    (is (= "This is my test handler.

   It depends on resources and a request, and produces a result."
           (description handler)))

    (is (= "This is my test handler." (short-description handler)))

    (is (= {:success? Boolean} (responses handler)))

    (is (= "$test$test$handler$:uri-arg" (path handler)))

    (is (= {s/Keyword s/Any :body-arg s/Keyword}
           (body handler)))

    (handler
     {:body {:body-arg :xx}
      :uri-args {:uri-arg 1}
      :query-params {:qp1 "x"}})
    (is (= {:uri-arg 1 :qp1 "x" :qp2 3 :body-arg :xx}
           (singleton @data-store)))))
