((nil . ((cider-jack-in-cmd . "clojure -M:local-dev:cljs")
         (cider-default-cljs-repl . custom)
         (cider-custom-cljs-repl-init-form . "
(do
  (require '[shadow.cljs.devtools.api :as shadow.api]
           '[shadow.cljs.devtools.server :as shadow.server])
  (shadow.server/start!)
  (shadow.api/watch :app)
  (shadow.api/nrepl-select :app))
"))))
