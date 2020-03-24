(ns main.app
  (:require [goog.dom :as gdom]
            [reagent.core :as reagent]
            [re-frame.core :as rf]))

(defn main-component []
  [:h1 "Hello, App"])

(defn init []
  (reagent/render-component [main-component]
                            (gdom/getElement "app")))

(defn on-reload []
  (rf/clear-subscription-cache!)
  (init))
