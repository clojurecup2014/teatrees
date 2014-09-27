(defproject teatrees "0.1.0"
  :description "3d tetris"
  :url ""
  :license {:name "EULA"
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
                 [secretary "1.2.0"]]
  :plugins [[lein-ring "0.8.11"]
            [lein-cljsbuild "1.0.3"]
            [lein-less "1.7.2"]
            [lein-pdo "0.1.1"]
            [lein-resource "0.3.6"]
            [lein-kibit "0.0.8"]]

  :ring {:handler teatrees.core/app}
  
  :less {:source-paths ["src/assets/less"]
         :target-path "public/css"}
  
  :cljsbuild { 
    :builds [{:id "dev"
              :source-paths ["src/teatrees-client"]
              :compiler {
                :output-to "public/js/teatrees.js"
                :output-dir "public/js"
                :optimizations :none
                :source-map true}}]}
  :resource {:resource-paths ["vendor"]
             :target-path "public"})
