(defproject tetris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[cider/cider-nrepl "0.14.0"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;; Need this to please Cider:
                 [org.clojure/tools.nrepl "0.2.12"]]
  :main tetris.core
  :aot :all)
