(ns fnhouse.handlers-test
  (:use clojure.test plumbing.core fnhouse.handlers)
  (:require
   [fnhouse.core :as fnhouse]
   [schema.core :as s]))

(deftest split-path-test
  (is (= ["a" "b" "c"] (split-path "/a/b//c//"))))

(deftest uri-arg-test
  (is (= :x (uri-arg ":x")))
  (is (nil? (uri-arg "x"))))

(deftest declared-uri-args-test
  (is (= #{:b :**} (declared-uri-args "/a/:b/c/:**/d"))))

(deftest route-and-method-test
  (is (= {:route "path/to/my/method"
          :method :get}
         (route-and-method "path/to/my/method$get"))))

(defnk ^:private $test$:handler$:uri-arg$POST :- (fnhouse/responses 200 "result" {:success? Boolean})
  "This is my test handler.

   It depends on resources and a request, and produces a result."
  {:auth-level #{:admin}}
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

(deftest nss->handlers-fn-test
  (let [annotation-fn (fn-> meta (select-keys [:auth-level :private]))
        handlers-fn (nss->handlers-fn {"my-test" 'fnhouse.handlers-test} annotation-fn)
        data-store (atom [])
        annotated-handlers (handlers-fn {:data-store data-store})]
    (letk [[handler
            [:info resources responses
             method path short-description description annotations
             [:request uri-args body query-params]]]
           (singleton annotated-handlers)]
      (is (= {:uri-arg s/Int :handler s/Str} uri-args))
      (is (= {s/Keyword s/Any :body-arg s/Keyword} body))
      (is (= {s/Keyword s/Any :qp1 s/Str (s/optional-key :qp2) s/Int} query-params))
      (is (= (fnhouse/responses 200 "result" {:success? Boolean}) responses))
      (is (= {s/Keyword s/Any :data-store s/Any} resources))
      (is (= "/my-test/test/:handler/:uri-arg" path))
      (is (= :post method))
      (is (= {:private true :auth-level #{:admin}} annotations))
      (is (= "This is my test handler." short-description))
      (is (= "This is my test handler.

   It depends on resources and a request, and produces a result."
             description))
      (assert (empty? @data-store))
      (handler
       {:body {:body-arg :xx}
        :uri-args {:uri-arg 1}
        :query-params {:qp1 "x"}})
      (is (= {:uri-arg 1 :qp1 "x" :qp2 3 :body-arg :xx}
             (singleton @data-store))))))
