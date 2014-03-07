(ns fnhouse.middleware-test
  (:use clojure.test plumbing.core fnhouse.middleware)
  (:require
   [schema.core :as s]
   [fnhouse.routes :as routes]
   [fnhouse.core :as fnhouse]
   [fnhouse.handlers :as handlers]
   [plumbing.graph :as graph]))

(defn var->annotated-handler [var & [resources]]
  {:info (handlers/var->info "" var (constantly nil))
   :handler (fn [request] (@var {:resources resources :request request}))})

(def resources
  {})

(s/defschema LowInput
  (s/pred integer? 'integer?))

(s/defschema HighOutput
  (s/pred integer? 'integer?))

(defnk $custom-coercion-handler$:id$POST
  {:responses {200 {:qp Long :body Long :uri-arg Long :high HighOutput}}}
  [[:request
    body :- LowInput
    [:uri-args id :- LowInput]
    [:query-params qp :- LowInput]]]
  {:body {:qp qp :body body :uri-arg id :high 100}})

(deftest custom-coercion-middleware-test
  (let [input-coercer (fn [schema]
                        (when (= schema LowInput)
                          (fn [request x]
                            (inc (if (string? x) (Long/parseLong x) x)))))
        output-coercer (fn [schema]
                         (when (= schema HighOutput)
                           (fn [request x] (dec x))))
        middleware (coercion-middleware input-coercer output-coercer)
        handler (:handler (middleware (var->annotated-handler #'$custom-coercion-handler$:id$POST resources)))]
    (is (= {:uri-arg 12 :qp 23 :body 34 :high 99}
           (:body (handler
                   {:uri "/custom-coercion-handler/11"
                    :request-method :post
                    :uri-args {:id "11"}
                    :query-params {:qp "22"}
                    :body 33}))))))


(defnk test$:some-id$route$:another-id$:interest-id$GET
  {:responses {200 s/Any}}
  [:request [:query-params qp-id :- Long] auth :- #{:test}
   [:uri-args [some-id :- Long another-id :- String interest-id :- interests/Interest]]]
  {:body {:some-id some-id :another-id another-id :qp-id qp-id :interest-id interest-id}})



(defnk test$:some-id$route$:another-id$:interest-id$POST
  {:responses {200 s/Any}}
  [:request [:query-params qp-id :- Long] auth :- #{:test}
   [:uri-args [some-id :- Long another-id :- String interest-id :- interests/Interest]]]
  {:body {:some-id some-id :another-id another-id :qp-id qp-id :interest-id interest-id}})

(defnk schema-check-handler$POST
  {:returns {200 {:some-id Long (s/optional-key :keyword) s/Keyword}}}
  [[:request auth :- #{:test}
    {query-params {}}
    [:body bool :- boolean long :- Long double :- Double string :- String
     {keyword :- s/Keyword nil}]]]
  {:body (merge {:some-id 123}
                (when-let [k (:keyword body)] {:keyword k})
                query-params)})


(defnk schema-check-handler$GET
  {:returns {200 {:some-id Long (s/optional-key :keyword) s/Keyword}}}
  [[:request auth :- #{:test}
    {query-params {}}]]
  {:body (merge {:some-id 123} query-params)})



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
