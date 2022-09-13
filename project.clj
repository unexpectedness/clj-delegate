(defproject clj-delegate "0.1.3"
  :description "TODO"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.clojars.unexpectedness/shuriken "0.14.28-SNAPSHOT"]
                 [robert/hooke "1.3.0"]
                 [org.flatland/ordered "1.5.4"]
                 [weaving "0.1.3"]
                 ;; TODO: explore dependencies and see how java bytecode is
                 ;; injected
                 [shady "0.2.0-SNAPSHOT"]]
  :aot [clj-delegate.class])
