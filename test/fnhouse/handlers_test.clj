(ns web.handlers-test
  (:use clojure.test plumbing.core plumbing.test web.handlers)
  (:require
   [clojure.set :as set]
   [plumbing.map :as map]))

(def route-spec-map {:uri1 {:uri2 {:get (constantly {:body :deep-fn})}
                            :uri3 {+wildcard+ {:id {:uri4 {:get (fn [& args] [:wild-fn args])}
                                                    :uri5 {:get (constantly :wild-fn-2)}}}
                                   :blarg {:get (constantly :blarg-fn)}
                                   :catchall {:get (fn [& args] [:catchall args])}}
                            :get (constantly :get-fn)
                            :post (constantly :post-fn)}
                     :get (fn [& args] [:catchall-get args])})

(deftest build-route-spec-test
  (is-= {:get :dummy
         :uri1 {:post :dummy
                :get :dummy
                :uri2 {:get :dummy}
                :uri3 {+wildcard+ {:id {:uri4 {:get :dummy}
                                        :uri5 {:get :dummy}}}
                       :blarg {:get :dummy}
                       :catchall {:get :dummy}}}}
        (map/map-leaves (constantly :dummy)
                        (build-route-spec route-spec-map nil))))

(deftest with-middleware-test
  (is-= {:uri1 {:uri2 {:get [:middle :deep-fn]}
                :get [:middle :get-fn]
                :post [:middle :post-fn]}}
        (with-middleware
          (fn [k] [:middle k])
          {:uri1 {:uri2 {:get :deep-fn}
                  :get :get-fn
                  :post :post-fn}})))

(deftest match-route-spec-test
  (let [route-spec (build-route-spec route-spec-map nil)]
    (is-= (match-route-spec route-spec ["uri1" "uri3" "1234" "uri5"] :get)
          [(safe-get-in route-spec [:uri1 :uri3 +wildcard+ :id :uri5 :get])
           "/uri1/uri3/1234/uri5" "/uri1/uri3/:id/uri5" {:id "1234"}])
    (is-= (match-route-spec route-spec ["uri1" "uri3" "blarg"] :get)
          [(safe-get-in route-spec [:uri1 :uri3 :blarg :get])
           "/uri1/uri3/blarg" "/uri1/uri3/blarg" {}])
    (is-= (match-route-spec route-spec ["uri1" "uri2"] :get)
          [(safe-get-in route-spec [:uri1 :uri2 :get]) "/uri1/uri2" "/uri1/uri2" {}])
    (is-= (match-route-spec route-spec ["uri1" "uri2" "JFDKLHJFDS"] :get)
          [(safe-get-in route-spec [:uri1 :uri2 :get]) "/uri1/uri2" "/uri1/uri2" {}])
    (is-= (match-route-spec route-spec ["blah" "blah" "blah"] :get)
          [(safe-get-in route-spec [:get]) "/" "/" {}])))

(deftest compile-handler-spec-test
  (let [h (compile-handler-spec route-spec-map nil)]
    (is-= :get-fn (h {:uri "/uri1" :request-method :get}))
    (is-= :post-fn (h {:uri "/uri1" :request-method :post}))
    (is-= {:body :deep-fn} (h {:uri "/uri1/uri2" :request-method :get}))
    (is-= :wild-fn-2 (h {:uri "/uri1/uri3/1234/uri5" :request-method :get}))
    (is-= :blarg-fn (h {:uri "/uri1/uri3/blarg" :request-method :get}))
    (is-= [:wild-fn [{:uri-arg ""
                      :uri-args {:id "1234"}
                      :matched-uri "/uri1/uri3/1234/uri4"
                      :matched-uri-skeleton "/uri1/uri3/:id/uri4"
                      :request-method :get
                      :uri "/uri1/uri3/1234/uri4"}]]
          (h {:uri "/uri1/uri3/1234/uri4" :request-method :get}))
    (is-= [:catchall [{:uri-arg "/more-crap-to-test-catchall/blah/blah/blah"
                       :uri-args {}
                       :matched-uri "/uri1/uri3/catchall"
                       :matched-uri-skeleton "/uri1/uri3/catchall"
                       :request-method :get
                       :uri "/uri1/uri3/catchall/more-crap-to-test-catchall/blah/blah/blah"}]]
          (h {:uri "/uri1/uri3/catchall/more-crap-to-test-catchall/blah/blah/blah" :request-method :get}))
    (is-= [:catchall-get [{:uri-arg "some-crap-to-test-catchall/blah/blah/blah"
                           :uri-args {}
                           :matched-uri "/"
                           :matched-uri-skeleton "/"
                           :request-method :get
                           :uri "/some-crap-to-test-catchall/blah/blah/blah"}]]
          (h {:uri "/some-crap-to-test-catchall/blah/blah/blah" :request-method :get}))
    (is-= 404 (:status (h {:uri "/some-crap-to-test-catchall/blah/blah/blah" :request-method :post})))
    (is-= {:body nil} (h {:uri "/uri1/uri2" :request-method :head}))))

(deftest compile-versioned-handler-specs-test
  (let [no-catchall (dissoc route-spec-map :get)
        h (compile-versioned-handler-specs
           {0 no-catchall
            1 (assoc (set/rename-keys no-catchall {:uri1 :urin})
                :api-version {:get (fnk [[:client api-version]] api-version)})}
           "0.0"
           nil)
        handle (fn [uri & [api-version]]
                 (h {:uri uri
                     :request-method :get
                     :query-params (if api-version
                                     {:api-version api-version}
                                     {})}))]
    (testing "api-version from query params"
      (is-= :get-fn (handle "/uri1"))
      (is-= :get-fn (handle "/uri1" "0.0"))
      (is-= 404 (:status (handle "/uri1" "1.0")))
      (is-= :get-fn (handle "/urin" "1.0"))
      (is-= :get-fn (handle "/urin" "1.1"))
      (is-= 404 (:status (handle "/urin" "0.0")))
      (is-= {:major 1 :minor 0} (handle "/api-version" "1.0"))
      (is-= {:major 1 :minor 1} (handle "/api-version" "1.1")))
    (testing "api-version from uri arg"
      (is-= :get-fn (handle "/0.0/uri1"))
      (is-= :get-fn (handle "/0.0/uri1" "1.0"))
      (is-= 404 (:status (handle "/1.0/uri1")))
      (is-= 404 (:status (handle "/0/uri1")))
      (is-= :get-fn (handle "/1.0/urin"))
      (is-= :get-fn (handle "/1.1/urin"))
      (is-= 404 (:status (handle "/0.0/urin")))
      (is-= {:major 1 :minor 0} (handle "/1.0/api-version"))
      (is-= {:major 1 :minor 1} (handle "/1.1/api-version")))))

(deftest user-agent-type-test
  (doseq [[device user-agents]
          {:iphone
           ["Prismatic/iPhone 1.0"
            "Prismatic/iPhone 2.0.0.b76/iPhone OS/7.0.4/iPhone5,1/(320 x 568)"
            "Prismatic/iPad Simulator 2.0.0.b76/iPhone OS/7.0.3/x86_64/(768 x 1024)"
            "Prismatic/iPad 2.0.0.b76/iPhone OS/7.0.3/iPad2,5/(768 x 1024)"
            "Prismatic/iPad 2.0.0.b76/iPhone OS/7.0.3/iPad4,1/(768 x 1024)"]
           :iphone-browser
           ["Mozilla/5.0 (iPad; CPU OS 7_0_3 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B511 Safari/9537.53"
            "Mozilla/5.0 (iPod touch; CPU iPhone OS 7_0_4 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B554a Safari/9537.53"
            "Mozilla/5.0 (iPhone; CPU iPhone OS 7_0_4 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B554a Safari/9537.53"]
           :web
           ["ANYTHING ELSE"]}
          user-agent user-agents]
    (testing [device user-agent]
      (is-= device
            (user-agent-type {"user-agent" user-agent})))))
