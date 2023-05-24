(ns kev.dev
  (:require
   [nrepl.cmdline]
   [shadow.cljs.devtools.api :as shadow.api]
   [shadow.cljs.devtools.server :as shadow.server]
   [kev.roam.data-import :as data-import]
   [shadow.cljs.devtools.cli :as shadow.cli]
   ))

(comment

  ;; rebuild db, dev version
  (build-roam-db {:roam-db-path "./public/db.edn"})

  (shadow-watch!)
  (require '[shadow.cljs.devtools.api :as shadow.api]
           '[shadow.cljs.devtools.server :as shadow.server])
  (shadow.server/start!)
  (shadow.api/compile :app)
  (shadow.api/watch :app)
  ;; don't run this, as it will switch the repl type
  (shadow.api/nrepl-select :app)
  )

(defn shadow-watch! []
  (require '[shadow.cljs.devtools.api :as shadow.api]
           '[shadow.cljs.devtools.server :as shadow.server])
  (shadow.server/start!)
  ;; I think this interferes with what is automatically sent?
;  (shadow.api/compile :app)
  (shadow.api/watch :app)
 ;;(shadow.api/nrepl-select :app)
  )

(defn build-roam-db [{:keys [roam-db-path]}]
  (assert roam-db-path "should have roam-db-path specified!")
  (data-import/write-datascript-db! roam-db-path))

(defn build
  "release-opts will by passsed into shadow. These are essentially the same
  as shadow cli options."
  [{:keys [app roam-db-path release-opts]}]
  ;; call shadow
  (shadow.api/release! app release-opts))


(defn dev-repl [{:keys [cider-args]}]
  (println "starting server")
  (shadow-watch!)
  ;; idk why, but there always seems to be issues on the initial start?
  (apply nrepl.cmdline/-main cider-args))
