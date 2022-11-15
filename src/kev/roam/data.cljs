(ns kev.roam.data
  "basically just the static data file where UI gets application data

  see data-import.clj for more info about keys available
  "
  (:require
   [re-frame.core :as rf]
   [re-frame.loggers :refer [console]]
   [promesa.core :as p]
   [datascript.core :as d])
  (:require-macros
   [kev.roam.data-import :as data-import]))

(def data-path (str (.-hostingAt js/window) "db.edn"))

(def db
  "gets set in set-db! will contain the db for the nodes and links"
  nil)

(rf/reg-event-db
 ::db
 (fn [db [_ ds-db]]
   (assoc db ::db ds-db)))

(rf/reg-sub
 ::db
 :->
 ::db)

(defn set-db! []
  (-> (js/fetch data-path)
      (p/then #(.text %))
      (p/then (fn [edn]
                (let [loaded-db (cljs.reader/read-string edn)]
                  (set! db loaded-db)
                  (rf/dispatch [::db db]))))
      (p/catch (fn [e] (console :error "db load error!" e)))))

(defn find-node-by [db attr value]
  (first
   (d/q '[:find [[pull ?e [:node/title :node/id :node/content]]]
          :in $ ?attr ?value
          :where [?e ?attr ?value]]
        db
        attr
        value)))

(comment

  (d/q '[:find [[pull ?e [:node/title :node/id]]]
         :where [?e :node/title _]]
       db)

  ;; => [#:node{:id "6927c0a1-4bfd-476f-bf81-a6644bc09394",
  ;;            :title "on lisp paul graham"}]

  (set-db!)

  (kev.roam.data/find-node-by
   db
   :node/id
   "b77d4578-bc50-42a8-94c4-b11d27f78837")

  (kev.roam.data/find-node-by
   db
   :node/title
   "keto")


  (d/q '[:find [[pull ?e [:node/title :node/id]] ...]
         :in $ ?title
         :where [?e :node/title ?title]]
       db
       "metformin")

  (d/q '[:find [[pull ?e [:node/title :node/id ]] ...]
          :in $ ?attr ?value
          :where [?e ?attr ?value]]
        db
        :node/title
        "metformin")


  )
