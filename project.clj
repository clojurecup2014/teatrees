(defproject teatrees "0.1.0"
  :description "3d tetris"
  :url ""
  :license {:name "Eclipse Public License 1.0"
            :url ""}
  :dependencies [[clj-time "0.7.0"]
                 [org.clojure/clojure "1.6.0"]
                 [ring/ring-core "1.3.0"]
                 [lib-noir "0.8.4"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ring-middleware-format "0.3.2"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.tobereplaced/lettercase "1.0.0"]
                 [om "0.6.5"]
                 [secretary "1.2.0"]
                 [clj-time "0.7.0"]
                 [org.clojure/clojurescript "0.0-2277"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.7.1"]
                 [prismatic/om-tools "0.3.2"]
                 [garden "1.2.1"]]
  :plugins [[lein-ring "0.8.11"]
            [lein-cljsbuild "1.0.3"]
            [lein-less "1.7.2"]
            [lein-pdo "0.1.1"]
            [lein-resource "0.3.6"]
            [lein-kibit "0.0.8"]]

  :ring {:handler teatrees.core/app}
  
  :less {:source-paths ["src/assets/less"]
         :target-path "resources/public/css"}
  
  :cljsbuild { 
    :builds [{:id "dev"
              :source-paths ["src/teatrees_client"]
              :compiler {
                :output-to "resources/public/js/teatrees.js"
                :output-dir "resources/public/js"
                :optimizations :none
                :source-map true}}]}
  :resource {:resource-paths ["vendor"]
             :target-path "resources/public"})
