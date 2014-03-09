(ns fnhouse.middleware-test
  (:use clojure.test plumbing.core fnhouse.middleware)
  (:require
   [schema.core :as s]
   [fnhouse.handlers :as handlers]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

(s/defschema Interest
  {:id Long
   :type s/Keyword
   :title String})

(s/defschema LowInput
  (s/pred integer? 'integer?))

(s/defschema HighOutput
  (s/pred integer? 'integer?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Data

(def clojure-interest
  {:id 123456
   :type :topic
   :title "Clojure"})

(def resources
  {:interests (map-from-vals :id [clojure-interest])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Methods

(defn var->annotated-handler [var & [resources]]
  {:info (handlers/var->info "" var (constantly nil))
   :handler (fn [request] (@var {:resources resources :request request}))})

(defn wrap [middleware var]
  (:handler (middleware (var->annotated-handler var))))

(defn interest-coercer [schema]
  (when (= schema Interest)
    (fn [request x]
      (if (or (string? x) (integer? x))
        (safe-get-in resources [:interests (long x)])
        x))))

(defn do-post [handler arg-map]
  (->> arg-map
       (merge {:uri "/post/to/handler/"
               :request-method :post
               :uri-args {}
               :query-params {}
               :body nil})
       handler
       :body))

(defn do-get [handler arg-map]
  (->> arg-map
       (merge {:uri "/get/to/handler/"
               :request-method :get
               :uri-args {}
               :query-params {}})
       handler
       :body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers

(defnk $custom-coercion-handler$:id$POST
  {:responses {200 {:body {:qp Long :body Long :uri-arg Long :high HighOutput}}}}
  [[:request
    body :- LowInput
    [:uri-args id :- LowInput]
    [:query-params qp :- LowInput]]]
  {:body {:qp qp :body body :uri-arg id :high 100}})

(defnk test$:some-id$route$:another-id$:interest-id$POST
  {:responses {200 {:body s/Any}}}
  [[:request
    [:query-params qp-id :- Long]
    [:uri-args some-id :- Long another-id :- String interest-id :- Interest]]]
  {:body {:some-id some-id :another-id another-id :qp-id qp-id :interest-id interest-id}})

(defnk schema-check-handler$POST
  {:responses {200 {:body {:some-id Long (s/optional-key :keyword) s/Keyword}}}}
  [[:request
    {query-params {}}
    [:body bool :- boolean long :- Long double :- Double string :- String
     {keyword :- s/Keyword nil}]]]
  {:body (merge {:some-id 123} (when keyword {:keyword keyword}) query-params)})

(defnk schema-check-handler$GET
  {:responses {200 {:body {:some-id Long (s/optional-key :keyword) s/Keyword}}}}
  [[:request {query-params {}}]]
  {:body (merge {:some-id 123} query-params)})

(defnk test$:some-id$route$:another-id$:interest-id$GET
  {:responses {200 {:body s/Any}}}
  [[:request
    [:query-params qp-id :- Long]
    [:uri-args some-id :- Long another-id :- String interest-id :- Interest]]]
  {:body {:some-id some-id :another-id another-id :qp-id qp-id :interest-id interest-id}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(deftest custom-coercion-middleware-test
  (let [input-coercer (fn [schema]
                        (when (= schema LowInput)
                          (fn [request x]
                            (-> x (?> (string? x) Long/parseLong) inc))))
        output-coercer (fn [schema]
                         (when (= schema HighOutput)
                           (fn [request x] (dec x))))
        middleware (coercion-middleware input-coercer output-coercer)
        handler (wrap middleware #'$custom-coercion-handler$:id$POST)]
    (is (= {:uri-arg 12 :qp 23 :body 34 :high 99}
           (do-post handler {:uri-args {:id "11"} :query-params {:qp "22"} :body 33})))))

(deftest coercion-middleware-test
  (let [middleware (coercion-middleware interest-coercer (constantly nil))]
    (is (= {:some-id 12 :another-id "34" :qp-id 666 :interest-id clojure-interest}
           (do-get (wrap middleware #'test$:some-id$route$:another-id$:interest-id$GET)
                   {:uri-args {:some-id "12" :another-id "34" :interest-id (:id clojure-interest)}
                    :query-params {:qp-id "666"}})))

    (let [get-schema-check-handler (wrap middleware #'schema-check-handler$GET)]
      (is (= {:some-id 123} (do-get get-schema-check-handler {})))
      (is (thrown? Throwable (do-get get-schema-check-handler {:query-params {:should-cause :exception}}))))

    (let [post-schema-check-handler (wrap middleware #'schema-check-handler$POST)]
      (is (= {:some-id 123 :keyword :keywordize-me}
             (do-post post-schema-check-handler
                      {:body {:bool true :long 1.0 :double 1.0 :string "cats" :keyword "keywordize-me"}})))
      (is (thrown? Throwable (do-post post-schema-check-handler
                                      {:body {:bool true :long "1" :double 1.0 :string "cats"}}))))))
