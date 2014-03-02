(ns fnhouse.core-test
  (:use clojure.test plumbing.core fnhouse.core)
  (:require
   [schema.core :as s]
   [plumbing.fnk.pfnk :as pfnk]))


(defnk $test$handler$:uri-arg :- {:success? Boolean}
  "This is my test handler.

   It depends on resources and a request, and produces a result."
  [[:request
    [:uri-args uri-arg :- s/Int]
    [:query-params qp1 :- s/Str {qp2 :- s/Int 3}]]
   [:resources data-store]]
  (swap! data-store
         conj
         {:uri-arg uri-arg
          :qp1 qp1
          :qp2 qp2})
  {:body {:success? true}})


(defnk $test$handler2 :- {:success? Boolean}
  "Description 2"
  [[:request body :- s/Str]
   [:resources data-store]]
  (swap! data-store conj body)
  {:body {:success? true}})

(deftest uri-args-test
  (is (= {s/Keyword s/Any :uri-arg s/Int}
         (uri-args #'$test$handler$:uri-arg))))

(deftest query-params-test
  (is (= {s/Keyword s/Any
          :qp1 s/Str
          (s/optional-key :qp2) s/Int}
         (query-params #'$test$handler$:uri-arg))))

(deftest private?-test
  (is (not (private? #'$test$handler$:uri-arg))))

(deftest description-test
  (is (= "This is my test handler.

   It depends on resources and a request, and produces a result."
         (description #'$test$handler$:uri-arg))))

(deftest short-description-test
  (is (= "This is my test handler."
         (short-description #'$test$handler$:uri-arg))))

(deftest responses-test
  (is (= {:success? Boolean}
         (responses #'$test$handler$:uri-arg))))

(deftest nss->handlers-fn-test
  (let [data-store (atom [])
        raw-svc (nss->handlers-fn {"test" 'fnhouse.core-test})
        service (raw-svc
                 {:data-store data-store})
        handler (last service)]
    (spy (meta service))
    (spy (meta raw-svc))
    (spy (meta handler))
    (handler
     {:uri-args {:uri-arg 1}
      :query-params {:qp1 "hi"
                     :qp2 100}})
    (is (= [{:uri-arg 1, :qp1 "hi", :qp2 100}])
        @data-store)))



;; (deftest compile-route-graph-test
;;   (let [hsb (compile-route-graph
;;              {:a {+fn-key+ (fnk [a b] (fnk [request] (+ a b (:foo request))))}
;;               :c {:d {+fn-key+ (fnk [b c] (fnk [request] (- b c (:doo request))))}}})]
;;     (is (= {:a s/Any :b s/Any :c s/Any s/Keyword s/Any} (pfnk/input-schema hsb)))
;;     (is (= {:a s/Any :c {:d s/Any}} (pfnk/output-schema hsb)))
;;     (letk [[a [:c d]] (hsb {:a 10 :b 100 :c 1000})]
;;       (is (thrown? Throwable (a {:foo 10})))
;;       (is (= 111 (a {:request {:foo 1}})))
;;       (is (= -910 (d {:request {:doo 10}}))))))


;; (defnk test1$route-1 {:methods #{:get}} [a b request]
;;   {:body (+ a b (:foo request))})

;; (defnk random-function-2 {:methods #{:post} :path "test1/nested/two"} [b c request]
;;   {:body (- b c (:doo request))})

;; (defnk test2$:some-id
;;   {:some-meta-tag :kittens
;;    :methods {:get {}}}
;;   [[:request uri some-meta-tag]]
;;   {:body {:uri uri :some-meta-tag some-meta-tag}})

;; (defnk test2$:some-id$route0
;;   {:methods {:get {}
;;              :post {}}}
;;   [[:request uri [:query-params a]]]
;;   {:body {:uri uri :a a}})

;; (defnk test2$:some-id$route1$:another-id
;;   {:methods {:get {}
;;              :post {}}}
;;   [[:request uri] b]
;;   {:body {:uri uri :b b}})

;; (deftest ns->route-graph-test
;;   (let [mw (fnk [:as resources]
;;              (fn [handler]
;;                (let [m (meta handler)]
;;                  (fnk [:as request]
;;                    (handler (-> request
;;                                 (assoc-when :some-meta-tag (:some-meta-tag m))
;;                                 (pci/assoc-in-when [:query-params :a]
;;                                                    (when-let [a (get-in request [:query-params :a])]
;;                                                      (.toUpperCase a)))))))))
;;         test-routes (:test2
;;                      ((ns->handlers-fn 'web.routes-test mw)
;;                       {:a "A" :b 100 :c 1000 :d 10000 :e 3 :z -1}))]
;;     (is (= (count test-routes) 1))
;;     (let [f0 (-> test-routes handlers/+wildcard+ :some-id)
;;           f1 (-> test-routes handlers/+wildcard+ :some-id :route0)
;;           f2 (-> test-routes handlers/+wildcard+ :some-id :route1 handlers/+wildcard+ :another-id)]
;;       (is (= #{:get :route0 :route1} (set (keys f0))))
;;       (is (= #{:get :post} (set (keys f1))))
;;       (is (= #{:get :post} (set (keys f2))))
;;       (is (= {:body {:uri "/ns/test2/3" :some-meta-tag :kittens}} ((:get f0) {:uri "/ns/test2/3"})))
;;       (is (= {:body {:uri "/ns/test2/1/route0" :a "BLAH"}} ((:get f1) {:uri "/ns/test2/1/route0" :query-params {:a "blah"}})))
;;       (is (= {:body {:uri "/ns/test2/1/route0" :a "BLAH"}} ((:post f1) {:uri "/ns/test2/1/route0" :query-params {:a "blah"}})))
;;       (is (= {:body {:uri "/ns/test2/1/route1/4" :b 100}} ((:get f2) {:uri "/ns/test2/1/route1/4"})))
;;       (is (= {:body {:uri "/ns/test2/1/route1/4" :b 100}} ((:post f2) {:uri "/ns/test2/1/route1/4"}))))))

;; (deftest map-leaves-to-key-test
;;   (is-= {:a {:stopped {:b 1 :c 2}} :stopped 3}
;;         (map-leaves-to-key :stop (fn [x] {:stopped x})
;;                            {:a {:stop {:b 1 :c 2}} :stop 3}))
;;   (is-= {:a {:get 1, :post 2, :d {:get 3}}}
;;         (map-leaves-to-key :fn identity {:a {:fn {:get 1 :post 2} :d {:fn {:get 3}}}})))

;; (deftest path->keyseq-test
;;   (is-= [+fn-key+] (path->keyseq ""))
;;   (is-= [:asdf +fn-key+] (path->keyseq "asdf"))
;;   (is-= [:asdf :b :cde +fn-key+] (path->keyseq "asdf/b/cde"))
;;   (is-= [:uri1 handlers/+wildcard+ :id :uri2 +fn-key+] (path->keyseq "/uri1/:id/uri2")))
