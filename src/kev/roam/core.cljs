(ns kev.roam.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [re-frame.loggers :refer [console]]
   [re-frame.db :as rf.db]
   [reitit.frontend.easy :as rfe]
   [reitit.frontend]
   [promesa.core :as p]
   [kev.roam.util]
   [kev.roam.node-frontend :as node-frontend]
   [kev.roam.graph :as graph]
   [kev.roam.data]
   [kev.roam.search :as kev.search]
   ["d3" :as d3]
   ["@mui/material" :as mui]
   ["@mui/material/CssBaseline" :default CssBaseline]
   ["@mui/material/styles" :refer [createTheme]]
   )
  (:require-macros
   [kev.roam.util :as util]))

(rf/reg-event-db
 ::route
 (fn [db [_ param]]
   (assoc db ::route-match param)))

;; this contains name under :data, :name
;; and :path-params contains more info
(rf/reg-sub
 ::route
 :->
 ::route-match)


(def theme
  (createTheme
   (clj->js
    ;; see https://fonts.google.com/ for options
    {:typography {:fontFamily "Lora" ; note: change index.html to download your font
                  :fontWeight 400
                  :fontStyle "normal"}
     :palette    {:mode       :light
                  #_#_#_#_
                  :text       {:primary   "#e0e0e0"
                               :secondary "#7f7f7f"}
                  :background {:default "#2b2525"}}})))

(defn node-page
  "looks up the node by title (bc that's what we're using as the path parameter)
  and renders it"
  [node-title]
  [:div
   "this will be the node page"
   node-title
   [:a {:href (rfe/href :route/home)}
    "home"]])

(defn app []
  (let [{{name :name} :data
         {node-title :title} :path-params :as taco}
        @(rf/subscribe [::route])
        kev-db @(rf/subscribe [:kev.roam.data/db])]
    (condp = name
      :route/node   [node-frontend/node-page kev-db node-title]
      :route/graph  [:> mui/Box
                    {:m 1.5}
                    [graph/node-graph "not actually passing data yet"]]
      :route/home   [node-frontend/node-page kev-db "home"]
      [:div "not found!"])))

(defn ^:dev/after-load render! []
;;;  (rf/dispatch-sync [:core/init])

  (-> (kev.roam.data/set-db!)
      (p/then (fn [_] (console :log "loaded db!"))))

  (rfe/start!
   (reitit.frontend/router
    [["/node/:title" {:name   :route/node
                      :handle #(rf/dispatch [::route %])}]
     ["/graph" {:name :route/graph
                :handle #(rf/dispatch [::route %])}]
     ["/" {:name   :route/home
           :handle #(rf/dispatch [::route %])}]]
    {:data {}})
   (fn [match _history]
     (some-> match :data :handle (apply [match])))
   {:use-fragment true})

  (rdom/render
   [:> mui/ThemeProvider
    {:theme theme}
    [:> CssBaseline]
    [:> mui/Box {:m 2}
     [app]]]
   (js/document.querySelector "#app")))

(defn ^:export init []
  (re-frame.loggers/set-loggers!
   {:debug (fn [& args] (apply js/console.debug (map clj->js args)))
    :log   (fn [& args] (apply js/console.log (map clj->js args)))
    :warn   (fn [& args] (apply js/console.warn (map clj->js args)))
    :error   (fn [& args] (apply js/console.error (map clj->js args)))
    :group   (fn [& args] (apply js/console.group (map clj->js args)))
    :groupEnd   (fn [& args] (apply js/console.groupEnd (map clj->js args)))})
  (render!))
