(defproject prismatic/fnhouse "0.1.0-SNAPSHOT"
  :description "Transform lightly-annotated functions into a full-fledged web service"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/Prismatic/fnhouse"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/plumbing "0.2.1"]]
  :profiles {:1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :dev {}}
  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]}
  :jvm-opts ^:replace []
  :aliases {"all" ["with-profile" "dev:dev,1.4"]})
