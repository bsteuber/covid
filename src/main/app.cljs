(ns main.app
  (:require [reagent.ratom :refer-macros [reaction]]
            [clojure.string :as str]
            [goog.dom :as gdom]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [testdouble.cljs.csv :as csv]
            [vimsical.re-frame.cofx.inject :as inject]))

  (def csv-filename "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv")

(rf/reg-event-db
 :set-data
 (fn [db [_ data]]
   (assoc db :data data)))

(defn parse-csv-data [data]
  (let [lines (csv/read-csv data)]
    (reduce (fn [m [date country province lat long confirmed recovered deaths]]
              (assoc-in m [date (str country
                                     (when-not (empty? province)
                                       (str " - " province)))]
                        {:cases  confirmed
                         :deaths deaths}))
            {}
            (rest lines))))

(rf/reg-fx
 :fetch-data
 (fn [_]
   (-> (js/fetch csv-filename)
       (.then (fn [response]
                (.text response)))
       (.then (fn [text]
                (rf/dispatch [:set-data (parse-csv-data text)])))
       (.catch (fn [e]
                 (js/console.error e))))))

(rf/reg-event-fx
 :maybe-fetch-data
 [(rf/inject-cofx ::inject/sub [:data])]
 (fn [{:keys [data]} _]
   (when-not data
     {:fetch-data true})))

(rf/reg-sub
 :data
 (fn [db]
   (:data db)))

(rf/reg-sub
 :past-days-str
 (fn [db _]
   (:past-days-str db "10")))

(rf/reg-sub
 :past-days
 :<- [:past-days-str]
 (fn [past-days-str _]
   (let [v (js/parseInt past-days-str)]
     (if (js/isNaN v)
       0
       v))))

(rf/reg-event-db
 :set-past-days-str
 (fn [db [_ x]]
   (assoc db :past-days-str x)))

(rf/reg-sub
 :dates
 :<- [:data]
 :<- [:past-days]
 (fn [[data days] _]
   (->> data
        keys
        (sort >)
        (take (+ days 8)))))

(rf/reg-sub
 :shown-dates
 :<- [:dates]
 :<- [:past-days]
 (fn [[dates days] _]
   (take days dates)))

(rf/reg-sub
 :all-countries
 :<- [:data]
 (fn [data _]
   (-> data
       first
       second
       keys
       sort)))

(rf/reg-sub
 :show-country-data
 (fn [db _]
   (get db :show-country?)))

(rf/reg-sub
 :show-country?
 (fn [db [_ country]]
   (get-in db [:show-country? country] true)))

(rf/reg-event-db
 :set-show-country
 (fn [db [_ country show?]]
   (assoc-in db [:show-country? country] show?)))

(rf/reg-event-db
 :select-all
 (fn [db]
   (dissoc db :show-country?)))

(rf/reg-event-fx
 :deselect-all
 [(rf/inject-cofx ::inject/sub [:all-countries])]
 (fn [{:keys [db all-countries]} _]
   {:db (assoc db :show-country?
               (zipmap all-countries
                       (repeat false)))}))

(rf/reg-sub
 :countries
 :<- [:all-countries]
 :<- [:show-country-data]
 (fn [[all-countries show-country-data] _]
   (filter (fn [country]
             (get show-country-data country true))
           all-countries)))

(rf/reg-sub
 :recent-country-cases
 :<- [:data]
 :<- [:dates]
 :<- [:past-days]
 (fn [[data dates days] [_ country]]
   (->> dates
        (map #(get-in data [% country :cases]))
        (take (+ days 8))
        vec)))

(rf/reg-sub-raw
 :recent-new-cases
 (fn [_ [_ country]]
   (reaction
    (let [country-data @(rf/subscribe [:recent-country-cases country])]
      (->> country-data
           (partition 2 1)
           (map (fn [[current before]]
                  (- current before)))
           vec)))))

(rf/reg-sub-raw
 :recent-new-cases-growth
 (fn [_ [_ country]]
   (reaction
    (let [country-data @(rf/subscribe [:recent-new-cases country])]
      (prn (count country-data))
      (->> country-data
           (partition 7 1)
           (map (fn [last-week]
                  (- (/ (first last-week)
                        (last last-week))
                     1)))
           vec)))))

(rf/reg-sub-raw
 :by-country
 (fn [_ _]
   (reaction
    (let [countries @(rf/subscribe [:countries])]
      (->> countries
           (map (fn [country]
                  [country
                   {:current-cases    @(rf/subscribe [:recent-country-cases country])
                    :new-cases        @(rf/subscribe [:recent-new-cases country])
                    :new-cases-growth @(rf/subscribe [:recent-new-cases-growth country])}]))
           (into {}))))))

(defn checkbox [id label value event]
  [:span.mr-4
   [:label.mr-1 {:for id}
    label]
   [:input {:id        id
            :type      :checkbox
            :checked   value
            :on-change (fn [e]
                         (rf/dispatch event))}]])

(defn settings []
  [:div
   [:label {:for :past-days-field}
    "Show last days: "]
   [:input {:id        :past-days-field
            :type      :text
            :value     @(rf/subscribe [:past-days-str])
            :on-change (fn [e]
                         (rf/dispatch [:set-past-days-str e.target.value]))}]
   [:div
    (doall
     (for [country @(rf/subscribe [:all-countries])]
       (let [show-country? @(rf/subscribe [:show-country? country])]
         ^{:key country}
         [checkbox country country show-country? [:set-show-country country (not show-country?)]])))]
   [:div
    [:button.btn.btn-sm.btn-outline-primary.mr-1
     {:on-click #(rf/dispatch [:select-all])}
     "Select All"]
    [:button.btn.btn-sm.btn-outline-primary
     {:on-click #(rf/dispatch [:deselect-all])}
     "Deselect all"]]])

(defn weekend? [date-str]
  (-> date-str
      js/Date.
      .getDay
      #{0 6}))

(defn format-percent [x]
  (str (-> x
           (* 100)
           js/Math.round)
       "%"))

(def left-border {:border-left "1px solid black"})

(defn full-table []
  (let [countries  @(rf/subscribe [:countries])
        dates      @(rf/subscribe [:shown-dates])
        by-country @(rf/subscribe [:by-country])]
    [:table.table
     [:thead
      [:tr.text-center
       [:th "Date"]
       (for [country countries]
         ^{:key country}
         [:th {:colSpan 3
               :style   left-border}
          country])]]
     [:tbody
      [:tr.text-right
       [:td]
       (for [country countries]
         ^{:key country}
         [:<>
          [:td {:style left-border}
           "Total Cases"]
          [:td
           "New Cases"]
          [:td
           "vs Last Week"]])]
      (for [[index date] (map-indexed list dates)]
        ^{:key date}
        [:tr.text-right
         (when (weekend? date)
           {:style {:background-color "#f0f9d1"}})
         [:td date]
         (for [country countries]
           ^{:key country}
           [:<>
            [:td {:style left-border}
             (get-in by-country [country :current-cases index])]
            [:td
             (get-in by-country [country :new-cases index])]
            [:td
             (format-percent (get-in by-country [country :new-cases-growth index]))]])])]]))

(defn main-component []
  [:div.container
   [:h1 "Recent Covid-19 Data"]
   [:div "Data Source: " [:a {:href "https://datahub.io/core/covid-19#data"}
                     "https://datahub.io/core/covid-19#data"]
    " (original data from the John Hopkins University)"]
   [:br]
   [settings]
   [:br]
   [full-table]])

(defn init []
  (rf/dispatch [:maybe-fetch-data])
  (rd/render [main-component]
             (gdom/getElement "app")))

(defn on-reload []
  (rf/clear-subscription-cache!)
  (js/console.clear)
  (init))
