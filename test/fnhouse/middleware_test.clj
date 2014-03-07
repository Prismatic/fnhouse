(ns fnhouse.middleware-test
  (:use clojure.test plumbing.core fnhouse.middleware)
  (:require
   [schema.core :as s]
   [plumbing.graph :as graph]))

(def resources
  nil)

(s/defschema LowInput
  (s/pred integer? 'integer?))

(defmethod input/coercer LowInput [_ resources]
  (fn [request x]
    (inc (if (string? x) (Long/parseLong x) x))))

(s/defschema HighOutput
  (s/pred integer? 'integer?))

(defmethod output/coercer HighOutput [_ resources]
  (fn [request x]
    (dec x)))

(defnk custom-coercion-handler$:id
  {:uri-args {:uri-arg LowInput}
   :methods {:post {:query-params {:qp LowInput}
                    :auth #{:test}
                    :body {:body LowInput}
                    :returns {200 {:body {:qp long :body long :uri-arg long
                                          :high HighOutput}}}}}}
  [[:query-params qp] [:body body] [:uri-args uri-arg]]
  {:body {:qp qp :body body :uri-arg uri-arg :high 100}})

(deftest custom-coercion-middleware-test
  (let [middleware (coercion-middleware resources)]
    (is-= {:uri-arg 12 :qp 23 :body 34 :high 99}
          (:body ((middleware #'custom-coercion-handler$:id)
                  {:uri "/ns/test3"
                   :request-method :post
                   :uri-args {:uri-arg "11"}
                   :query-params {:qp "22"}
                   :body {:body 33}})))))

(defnk test$:some-id$route$:another-id$:interest-id
  {:uri-args {:some-id long :another-id String :interest-id interests/Interest}
   :methods {:get {:query-params {:qp-id long}
                   :auth #{:test}
                   :returns {200 {:body s/Any}}}
             :post {:auth #{:test}
                    :returns {200 {:body s/Any}}}}}
  [[:uri-args some-id another-id interest-id] [:query-params qp-id]]
  {:body {:some-id some-id :another-id another-id :qp-id qp-id :interest-id interest-id}})


(defnk schema-check-handler
  {:methods {:get {:auth #{:test}
                   :returns {200 {:body {:some-id long}}}}
             :post {:auth #{:test}
                    :body {:bool boolean
                           :long long
                           :double double
                           :string String
                           (s/optional-key :keyword) clojure.lang.Keyword}
                    :returns {200 {:body {:some-id long
                                          (s/optional-key :keyword) clojure.lang.Keyword}}}}}}
  [{query-params {}} {body {}}]
  {:body (merge {:some-id 123}
                (when-let [k (:keyword body)] {:keyword k})
                query-params)})

(deftest coercion-middleware-test
  ;; TODO: this test is a little bit dirty because the handler doesn't go through it's normal transform.
  (let [middleware (coercion-middleware resources)]
    (is-= {:some-id 12 :another-id "34" :qp-id 666 :interest-id resources-interests/home-interest}
          (:body ((middleware #'test$:some-id$route$:another-id$:interest-id)
                  {:uri (format "/ns/test/12/route/34/personal:")
                   :request-method :get
                   :uri-args {:some-id "12" :another-id "34" :interest-id "personal:" }
                   :query-params {:qp-id "666"}})))
    (is-= {:some-id 123}
          (:body ((middleware #'schema-check-handler)
                  {:uri "/ns/test3"
                   :request-method :get
                   :uri-args {}
                   :query-params {}
                   :body {}})))
    (is-= {:some-id 123 :keyword :should-get-keywordized}
          (:body ((middleware #'schema-check-handler)
                  {:uri "/ns/test3"
                   :request-method :post
                   :uri-args {}
                   :query-params {}
                   :body {:bool true :long 1.0 :double 1.0 :string "cats" :keyword "should-get-keywordized"}})))
    (is (thrown? Throwable
                 ((middleware #'schema-check-handler)
                  {:uri "/ns/test3"
                   :request-method :get
                   :uri-args {}
                   :query-params {:should-cause :exception}})))
    (is (thrown? Throwable
                 ((middleware #'schema-check-handler)
                  {:uri "/ns/test3"
                   :request-method :post
                   :uri-args {}
                   :query-params {}
                   :body {:bool true :long "1" :double 1.0 :string "cats"}})))))
