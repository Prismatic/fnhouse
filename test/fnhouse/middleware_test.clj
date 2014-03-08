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

(s/defschema Interest
  {:id Long
   :type s/Keyword
   :title String})

(def clojure-interest
  {:id 123456
   :type :topic
   :title "Clojure"})

(def resources
  {:interests (map-from-vals :id [clojure-interest])})

(s/defschema LowInput
  (s/pred integer? 'integer?))

(s/defschema HighOutput
  (s/pred integer? 'integer?))

(defnk $custom-coercion-handler$:id$POST
  {:responses {200 {:body {:qp Long :body Long :uri-arg Long :high HighOutput}}}}
  [[:request
    body :- LowInput
    [:uri-args id :- LowInput]
    [:query-params qp :- LowInput]]]
  {:body {:qp qp :body body :uri-arg id :high 100}})

(defn wrap [middleware var]
  (:handler (middleware (var->annotated-handler var))))

(deftest custom-coercion-middleware-test
  (let [input-coercer (fn [schema]
                        (when (= schema LowInput)
                          (fn [request x]
                            (inc (if (string? x) (Long/parseLong x) x)))))
        output-coercer (fn [schema]
                         (when (= schema HighOutput)
                           (fn [request x] (dec x))))
        middleware (coercion-middleware input-coercer output-coercer)
        handler (wrap middleware #'$custom-coercion-handler$:id$POST resources)]
    (is (= {:uri-arg 12 :qp 23 :body 34 :high 99}
           (:body (handler
                   {:uri "/custom-coercion-handler/11"
                    :request-method :post
                    :uri-args {:id "11"}
                    :query-params {:qp "22"}
                    :body 33}))))))






(defnk test$:some-id$route$:another-id$:interest-id$POST
  {:responses {200 {:body s/Any}}}
  [[:request
    [:query-params qp-id :- Long]
    [:uri-args some-id :- Long another-id :- String interest-id :- Interest]]]
  {:body
   {:some-id some-id
    :another-id another-id
    :qp-id qp-id
    :interest-id interest-id}})

(defnk schema-check-handler$POST
  {:returns {200 {:body {:some-id Long (s/optional-key :keyword) s/Keyword}}}}
  [[:request
    {query-params {}}
    [:body bool :- boolean long :- Long double :- Double string :- String
     {keyword :- s/Keyword nil}]]]
  {:body (merge {:some-id 123}
                (when keyword {:keyword keyword})
                query-params)})


(defnk schema-check-handler$GET
  {:returns {200 {:body {:some-id Long (s/optional-key :keyword) s/Keyword}}}}
  [[:request {query-params {}}]]
  {:body (merge {:some-id 123} query-params)})


(defnk test$:some-id$route$:another-id$:interest-id$GET
  {:responses {200 {:body s/Any}}}
  [[:request
    [:query-params qp-id :- Long]
    [:uri-args some-id :- Long another-id :- String interest-id :- Long #_Interest]]]
  (spy {:body
        {:some-id some-id
         :another-id another-id
         :qp-id qp-id
         :interest-id interest-id}}))


(deftest coercion-middleware-test
  (let [input-coercer (constantly nil)
        #_(fn [schema]
            (when (= schema Interest)
              (spy schema)
              (fn [request x]
                (spy request)
                (if (or (string? x) (integer? x))
                  (doto
                      (safe-get-in resources [:interests (long x)])
                    (assert "not found")
                    )
                  x))))
        output-coercer (constantly nil)
        middleware (coercion-middleware input-coercer output-coercer)]
    (is (= {:some-id 12 :another-id "34" :qp-id 666 :interest-id (:id clojure-interest)}
           (:body ((wrap middleware #'test$:some-id$route$:another-id$:interest-id$GET)
                   (spy
                    {:uri (format "/ns/test/12/route/34/personal:")
                     :request-method :get
                     :uri-args {:some-id "12" :another-id "34" :interest-id (:id clojure-interest)}
                     :query-params {:qp-id "666"}})))))
    (let [schema-check-handler (wrap middleware #'schema-check-handler$GET)]
      (is (= {:some-id 123}
             (:body (schema-check-handler
                     {:uri "/ns/test3"
                      :request-method :get
                      :uri-args {}
                      :query-params {}
                      :body {}}))))
      (is (= {:some-id 123 :keyword :should-get-keywordized}
             (:body (schema-check-handler
                     {:uri "/ns/test3"
                      :request-method :post
                      :uri-args {}
                      :query-params {}
                      :body {:bool true :long 1.0 :double 1.0 :string "cats" :keyword "should-get-keywordized"}}))))
      #_(is (thrown? Throwable
                     (schema-check-handler
                      {:uri "/ns/test3"
                       :request-method :get
                       :uri-args {}
                       :query-params {:should-cause :exception}})))
      #_(is (thrown? Throwable
                     (schema-check-handler
                      {:uri "/ns/test3"
                       :request-method :post
                       :uri-args {}
                       :query-params {}
                       :body {:bool true :long "1" :double 1.0 :string "cats"}}))))))
