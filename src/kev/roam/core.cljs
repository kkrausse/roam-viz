(ns kev.roam.core
  (:require
   [datascript.core :as d]
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
   [kev.roam.search :as roam.search]
   ["d3" :as d3]
   ["@mui/material" :as mui]
   ["@mui/material/CssBaseline" :default CssBaseline]
   ["@mui/material/styles" :refer [createTheme]]
   [reagent.core :as r])
  (:require-macros
   [kev.roam.util :as util]))

(prn "wat")
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
    {:typography {:fontFamily "verdana" ; note: change index.html to download your font
                  :fontWeight 400
                  :fontSize 12
                  :fontStyle "normal"}
     :palette    {:mode       :light
                  #_#_#_#_
                  :text       {:primary   "#e0e0e0"
                               :secondary "#7f7f7f"}
                  :background {:default "#2b2525"}}})))

(defn search-box [roam-db]
  [roam.search/search-box
   (->> (d/q '[:find [[pull ?e [:nodes/title :nodes/id :nodes/content]] ...]
               :where [?e :nodes/title ?title]
               [?e :nodes/content ?content]]
             roam-db)
        (map (fn [{:nodes/keys [title content]}]
               {:title title
                :text content
                :on-select (fn [_]
                             (console :log "selected" title)
                             (rfe/push-state :route/node {:title title}))})))] )

(defn app []
  (let [roam-db @(rf/subscribe [:kev.roam.data/db])]
    [:div
     [search-box roam-db]
     (let [{{name :name}        :data
            {node-title :title
             subheading :subheading} :path-params :as taco} @(rf/subscribe [::route])]
       (condp = name
         :route/node       [node-frontend/node-page roam-db :node-title node-title]
         :route/subheading [node-frontend/node-page roam-db
                            :node-title node-title
                            :subheading subheading]
         :route/home       [node-frontend/node-page roam-db :node-title "home"]
         :route/graph      [:> mui/Box {:m 1.5}
                            [graph/node-graph roam-db]]
         [:div "not found!"]))]))

(def functional-compiler (r/create-compiler {:function-components true}))

(comment

  (reitit.frontend.history)
  )
(defn ^:dev/after-load render! []
;;;  (rf/dispatch-sync [:core/init])
  (-> (kev.roam.data/set-db!)
      (p/then (fn [_] (console :log "loaded db!"))))

  (rfe/start!
   (reitit.frontend/router
    [["/node/:title"
      {:name   :route/node
       :handle #(rf/dispatch [::route %])}]
     ["/node/:title/:subheading"
      {:name   :route/subheading
       :handle #(rf/dispatch [::route %])}]
     ["/graph" {:name :route/graph
                :handle #(rf/dispatch [::route %])}]
     ["/" {:name   :route/home
           :handle #(rf/dispatch [::route %])}]]
    {:data {}
     })
   (fn [match _history]
     (some-> match :data :handle (apply [match])))
   {:use-fragment true
    :ignore-anchor-click? (fn [& args]
                            (console :log "ignore anchor click" args)
                            false)
    })

  (rdom/render
   [:> mui/ThemeProvider
    {:theme theme}
    [:> CssBaseline]
    [:div {:style {:margin (theme.spacing 1)}}
     [app]]]
   (js/document.querySelector "#app")
   functional-compiler))

(defn ^:export init []
  (re-frame.loggers/set-loggers!
   {:debug (fn [& args] (apply js/console.debug (map clj->js args)))
    :log   (fn [& args] (apply js/console.log (map clj->js args)))
    :warn   (fn [& args] (apply js/console.warn (map clj->js args)))
    :error   (fn [& args] (apply js/console.error (map clj->js args)))
    :group   (fn [& args] (apply js/console.group (map clj->js args)))
    :groupEnd   (fn [& args] (apply js/console.groupEnd (map clj->js args)))})
  (render!))
