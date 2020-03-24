(ns main.tests
  (:require [cljs.test :refer [run-tests]]
            [cljs-test-display.core :as test-display]))

(defn run []
  (js/console.clear)
  (run-tests
   (test-display/init! "app")
   ; 'my.tests.ns
   ))
