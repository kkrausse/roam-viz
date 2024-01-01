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
   (d/q '[:find [[pull ?e [:nodes/title :nodes/id :nodes/content]]]
          :in $ ?attr ?value
          :where [?e ?attr ?value]]
        db
        attr
        value)))

(defn find-top-node [db node-id]
  (first
   (d/q '[:find [[pull ?e [*]]]
          :in $ ?id
          :where
          [?this-e :nodes/id ?id]
          [?this-e :nodes/file ?file]
          [?e :nodes/file ?file]
          [?e :nodes/level 0]]
        db
        node-id)))

(comment

  (-> '[:find [[pull ?e [*] #_[:nodes/title :nodes/id :nodes/properties]] #_...]
        :where
        [?e :nodes/title _]
        [?e :nodes/properties ?p]
        [?e :nodes/title "ai papers TODO"]]
      (d/q db)
      first
      (->>
       ;; :nodes/id
       ;; (find-top-node db)
       (remove (fn [[k v]]
                 (or (= "" v)
                     (= k :nodes/content))))
       (into {}
         ))
      ;(map first)
      )

  (set-db!)

  (kev.roam.data/find-node-by
   db
   :nodes/id
   "b77d4578-bc50-42a8-94c4-b11d27f78837")

  (kev.roam.data/find-node-by
   db
   :nodes/title
   "keto")


  (d/q '[:find [[pull ?e [:nodes/title :nodes/id :nodes/created]] ...]
         :in $ ?title
         :where [?e :nodes/title ?title]]
       db
       "metformin")

  (d/q '[:find [[pull ?e [:nodes/title :nodes/id ]] ...]
          :in $ ?attr ?value
          :where [?e ?attr ?value]]
        db
        :nodes/title
        "metformin")


  )
