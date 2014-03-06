(ns fnhouse.routes-test
  (:use clojure.test plumbing.core fnhouse.routes)
  (:require
   [fnhouse.core :as fnhouse]
   [fnhouse.handlers :as handlers]
   [schema.core :as s]))

(deftest prefix-lookup-test
  (let [leaf (fn [x] {:get x})
        result (fn [match-result value]
                 {:match-result match-result
                  :handler value})
        node {+single-wildcard+ (leaf 7)
              "a" {+multiple-wildcard+ (leaf 1)
                   "b" {"x" (leaf 2)
                        "w" (leaf 5)}

                   +single-wildcard+
                   {"y" (leaf 3)
                    "z" (leaf 4)}}}
        lookup #(prefix-lookup node (conj % :get))]


    (testing "basic, no wildcards"
      (is (= (result ["a" "b" "x"] 2)
             (lookup ["a" "b" "x"]))))

    (testing "lookup miss"
      (is (nil? (lookup ["absent" "key"]))))

    (testing "single-wildcard"
      (is (= (result ["a" "bingo" "y"] 3)
             (lookup ["a" "bingo" "y"]))))

    (testing "multiple wildcard"
      (is (= (result ["a" ["1" "2" "3"]] 1)
             (lookup ["a" "1" "2" "3"]))))

    (testing "match consists of a single-wildcard"
      (is (= (result ["a"] 7)
             (lookup ["a"]))))

    (testing "prefer literal to wildcard"
      (is (= (result ["a" "b" "w"] 5)
             (lookup ["a" "b" "w"]))))

    (testing "prefer single- to multiple-wildcard"
      (is (= (result ["a" "bingo" "y"] 3)
             (lookup ["a" "bingo" "y"])))
      (is (= (result ["a" ["bingo" "multiple" "y"]] 1)
             (lookup ["a" "bingo" "multiple" "y"]))))))

(deftest uri-arg-test
  (is (nil? (uri-arg "x")))
  (is (= :x (uri-arg ":x"))))

(deftest match-segment-test
  (is (= +single-wildcard+ (match-segment ":x")))
  (is (= +multiple-wildcard+ (match-segment ":*")))
  (is (= "y" (match-segment "y"))))

(deftest uri-arg-map-test
  (is (= {1 :x 3 :y 5 :z}
         (uri-arg-map ["a" ":x" "*" ":y" "c" ":z"]))))

(deftest realize-uri-args-test
  (is (= {:x "x-value"
          :y "y-value"
          :z "z-value"}
         (realize-uri-args
          {1 :x 3 :y 5 :z}
          ["a" "x-value"
           "b" "y-value"
           "c" "z-value"]))))

(deftest request->path-seq-test
  (is (= ["a" "b" "c" :get]
         (request->path-seq {:uri "/a/b//c/" :request-method :get}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Handlers

(defmacro capture-wildcard [s]
  [:* :as ] :- s/Str)

(defnk $GET :- (fnhouse/responses 200 s/Any)
  []
  {:body "You've hit the root"})

(defnk $a$:uri-arg$b$:**$GET :- (fnhouse/responses 200 s/Any)
  [[:request
    [:uri-args [:* ] uri-arg :- s/Int]]]
  {:uri-arg uri-arg :wild-card *})

(defnk $x$:a$y$:b$POST :- (fnhouse/responses 200 s/Any)
  [[:request
    [:uri-args a :- s/Str b :- s/Str]]]
  {:a a :b b})


(deftest root-handler-test
  (let [svc-fn  (handlers/nss->handlers-fn {"test" 'fnhouse.routes-test})
        handlers (svc-fn {})
        h (root-handler handlers)]

    (is (= {:body "You've hit the root"}
           (h {:request-method :get
               :uri "/test"})))

    (is (= {:uri-arg "1337" :wild-card ["wild" "card"]}
           (h {:request-method :get
               :uri "/test/a/1337/b/wild/card"})))

    (is (= {:a "a-match" :b "b-match"}
           (h {:request-method :post
               :uri "/test/x/a-match/y/b-match/"})))

    (is (= {:status 404 :body "Not found."}
           (h {:request-method :get
               :uri "this/does/not/exist"})))))
