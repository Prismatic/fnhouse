(ns fnhouse.core-test
  (:use clojure.test plumbing.core fnhouse.core)
  (:require
   [schema.core :as s]))

(deftest responses-test
  (let [responses-schema (responses
                          100 "hi" {:body s/Str}
                          200 {:body s/Int}
                          300 "x" {:body s/Keyword})]
    (is (= {100 {:description "hi" :value {:body s/Str}}
            200 {:description "" :value {:body s/Int}}
            300 {:description "x" :value {:body s/Keyword}}}
           (:status->response-schema responses-schema)))
    (testing "body does not match schema"
      (is (not (nil? (s/check responses-schema {:status 100 :body 3})))))
    (testing "body does match"
      (is (nil? (s/check responses-schema {:status 200 :body 3}))))))
