(ns kev.dev
  (:require [kev.roam.data-import :as data-import]))

(comment

  (require 'portal.api)
  (def portal (portal.api/open))

  (do portal)

  (prn "taco")

  (portal.api/inspect
   [^{:portal.viewer/default :portal.viewer/hiccup}
                       [:h1 "hello"]
                       ^{:portal.viewer/default :portal.viewer/hiccup}
    [:h1 "world"]])

  (data-import/write-datascript-db! "./public/db.edn")
  ;; rebuild db, dev version
  (build-roam-db {:roam-db-path "./public/db.edn"})

  (shadow-watch!)
  (require '[shadow.cljs.devtools.api :as shadow.api]
           '[shadow.cljs.devtools.server :as shadow.server])
  (shadow.server/start!)
  (shadow.api/watch :app)
  (shadow.api/compile :app)

  (shadow.api/nrepl-select :app)
  )


(defn build-roam-db [{:keys [db-out-path roam-db-path] :as build-args}]
  (assert roam-db-path "should have roam-db-path specified!")
  (println "building with args" build-args)
  (time (data-import/write-datascript-db! db-out-path roam-db-path)))

(defn build
  "release-opts will by passsed into shadow. These are essentially the same
  as shadow cli options."
  [{:keys [app roam-db-path release-opts]}]
  ;; call shadow
  ((requiring-resolve 'shadow.cljs.devtools.api/release!) app release-opts))
