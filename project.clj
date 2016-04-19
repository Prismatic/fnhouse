(defproject tiensonqin/fnhouse-hacks "0.2.2-SNAPSHOT"
  :description "Transform lightly-annotated functions into a full-fledged web service"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/plumatic/fnhouse"
  :dependencies [[prismatic/plumbing "0.4.3" :exclusions [prismatic/schema]]
                 [prismatic/schema "1.0.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]]
                   :global-vars {*warn-on-reflection* true}}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0-RC1"]]}}
  :aliases {"all" ["with-profile" "dev:dev,1.5:dev,1.7:dev,1.8"]}
  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]})
