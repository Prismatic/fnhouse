(ns fnhouse.route-test
  (:use clojure.test plumbing.core fnhouse.route)
  (:require
   [schema.core :as s]))

(deftest split-path-test
  (is (= [:a :b :c] (split-path "/a/b//c//"))))

(deftest all-splits-test
  (is (= [[[:a] [:b]]] (all-splits [:a :b])))
  (is (= [[[1] [2 3 4]]
          [[1 2] [3 4]]
          [[1 2 3] [4]]]
         (all-splits [1 2 3 4])))
  (is (= [] (all-splits [:a])))
  (is (= [] (all-splits []))))

(deftest prefix-lookup-test
  (let [leaf (fn [x] {+handler-key+ x})
        result (fn [match-result value]
                 {:match-result match-result
                  :handler value})
        node {+single-wildcard+ (leaf 7)

              :a {+multiple-wildcard+
                  {:y (leaf 8)
                   :w (leaf 1)
                   :v {+multiple-wildcard+ (leaf 6)}}

                  :b {:x (leaf 2)
                      :w (leaf 5)}

                  +single-wildcard+
                  {:y (leaf 3)
                   :z (leaf 4)}}}]

    (testing "basic, no wildcards"
      (is (= (result [:a :b :x] 2)
             (prefix-lookup node [:a :b :x]))))

    (testing "lookup miss"
      (is (nil? (prefix-lookup node [:absent :key]))))

    (testing "single wildcard"
      (is (= (result [:a :bingo :y] 3)
             (prefix-lookup node [:a :bingo :y]))))

    (testing "multiple wildcard"
      (is (= (result [:a [:1 :2 :3] :w] 1)
             (prefix-lookup node [:a :1 :2 :3 :w]))))

    (testing "match consists of a single wildcard"
      (is (= (result [:a] 7)
             (prefix-lookup node [:a]))))

    (testing "prefer literal to wildcard"
      (is (= (result [:a :b :w] 5)
             (prefix-lookup node [:a :b :w]))))

    (testing "prefer single to multiple wildcard"
      (is (= (result [:a :bingo :y] 3)
             (prefix-lookup node [:a :bingo :y])))
      (is (= (result [:a [:bingo :multiple] :y] 8)
             (prefix-lookup node [:a :bingo :multiple :y]))))

    (testing "two multiple wildcards"
      (is (= (result [:a [:1 :2 :3] :v [:4 :5 :6]] 6)
             (prefix-lookup node [:a :1 :2 :3 :v :4 :5 :6]))))

    (testing "multiple wildcard tests"
      (let [wild-node {+multiple-wildcard+ (leaf 1)
                       :a {+multiple-wildcard+ {+multiple-wildcard+ (leaf 2)}}}]

        (testing "match consists of a multiple wildcard"
          (is (= (result [[:x]] 1)
                 (prefix-lookup wild-node [:x]))))

        (testing "multiple wildcard must match at least 1 item"
          (is (nil? (prefix-lookup wild-node []))))

        (testing "multiple wildcard is greedy"
          (let [big-seq (range 100)]
            (is (= (result [(butlast big-seq) [(last big-seq)]] 2))
                (prefix-lookup wild-node big-seq))))))))
