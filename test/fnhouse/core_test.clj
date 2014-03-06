(ns fnhouse.core-test
  (:use clojure.test plumbing.core fnhouse.core)
  (:require
   [schema.core :as s]))

(deftest responses-test
  (let [responses-schema (responses
                          100 "hi" s/Str
                          200 s/Int
                          300 "x" s/Keyword)]
    (is (= {100 {:description "hi" :value s/Str}
            200 {:description "" :value s/Int}
            300 {:description "x" :value s/Keyword}}
           (:status->response-schema responses-schema)))
    (testing "body does not match schema"
      (is (not (nil? (s/check responses-schema {:status 100 :body 3})))))
    (testing "body does match"
      (is (nil? (s/check responses-schema {:status 200 :body 3}))))))
