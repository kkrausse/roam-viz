(ns kev.dev
  (:require
   [nrepl.cmdline]
   [shadow.cljs.devtools.api :as shadow.api]
   [shadow.cljs.devtools.server :as shadow.server]

   [shadow.cljs.devtools.cli :as shadow.cli]
   ))

(comment

  (prn "wut")
  (shadow-watch!)
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

(defn build
  "release-opts will by passsed into shadow. These are essentially the same
  as shadow cli options."
  [{:keys [app release-opts]}]
  (shadow.api/release! app release-opts))


(defn dev-repl [{:keys [cider-args]}]
  (println "starting server")
  (shadow-watch!)
  ;; idk why, but there always seems to be issues on the initial start?
  (apply nrepl.cmdline/-main cider-args))
