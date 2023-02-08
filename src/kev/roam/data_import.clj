(ns kev.roam.data-import
  (:require
   [org-parser.parser :as org-parser]
   [next.jdbc :as jdbc]
   [clojure.set :as set]
   [clojure.math :as math]
   [datascript.core :as d]
   ))

(def jdbc-url "jdbc:sqlite:/Users/kevinkrausse/.emacs.d/.local/cache/org-roam.db")

;; TODO: how can we make this extensible to others' configurations?
;; maybe they want a custom query
(defn all-links-with-source-nodes
  "queries org roam db for all nodes satisfying:
    - not containing a link to an ekata thing
    - not containing a link to 'private'
  And finds all links from one node in that set to other nodes in that set.


  "
  [jdbc-url]
  (let [jdbc-url jdbc-url
        ds (jdbc/get-datasource {:jdbcUrl jdbc-url})]
    (jdbc/execute! ds ["
with
usable_nodes
as (select * from nodes as use_nodes where

   -- ignore any pages linking to private, ekata, or taxbit
   -- or those with 'ekata' in the title
    not exists (
        select * from links
        inner join nodes as dest ON dest.id=links.dest
        where -- source is node specified
        links.source=use_nodes.id and
        (
        -- dest is anything ekata, taxbit, or private...
        INSTR(dest.title, 'ekata') > 0
        or INSTR(dest.title, 'taxbit') > 0
        or dest.title = '\"private\"'
        )
    )
    and
    INSTR(use_nodes.title, 'ekata') <= 0
)
select * from usable_nodes
left join links on usable_nodes.id = links.source
AND (links.dest != '\"id\"' OR links.dest in (select id from usable_nodes))
"] {:max-rows 100000})))

#_
(defn get-all-nodes
  "change strategy: WAS doing sql query to get all data. NOW I think clojure for
  filtering might allow more introspection which could be better")

(defn remove-quotes [s]
  (subs s
        (if (= (first s) \") 1 0)
        (if (= (last s) \") (dec (count s)) (count s))))

(defn power-law-step
  [links id->vals]
  (reduce
   (fn [newid-vals [id v]]
     (let [links-from (filter #(= id (:source %))
                              links)
           ;; pretending like it has a link to itself
           val-add (float (/ v (+ 1 (count links-from))))]
       (merge-with +
                   newid-vals
                   (into {id (max 2.0 val-add)}
                     (map (fn [{:keys [source target]}]
                            [target val-add]))
                     links-from))))
   (into {} (map (fn [[k v]] [k 0])) id->vals)
   id->vals))

(defn node-value
  "number of links into a node plus the value of the links into those nodes"
  [id links nodes]
  (let [links-to (filter #(= id (:target %)) links)]
    (reduce +)))

(defn roam-file-name->time-string [fname]
  (second (re-matches #".*/(\d{14})-.*" fname)))

(defn ->nodes+links []
  (let [links+source (all-links-with-source-nodes jdbc-url)
        nodes (->> links+source
                   (filter (comp some? :nodes/id))
                   (map (fn [{id :nodes/id :as x}]
                          [(remove-quotes id)
                           (-> x
                               (update :nodes/properties
                                       (fn [props]
                                         (prn props)
                                         (->> props
                                              read-string
                                              (map (fn [[k _dot v]]
                                                     [k v]))
                                              (into {}))))
                               (update :nodes/title remove-quotes)
                               (update :nodes/id remove-quotes)
                               (update :nodes/file remove-quotes))]))
                   ;; filter out dailies--unfortunate, but less so than using regex in sqlite
                   (filter (fn [[_ {:nodes/keys [title]}]]
                             (nil? (re-find #"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" title))))
                   (into {}))
        links (->> links+source
                   (map #(select-keys % [:links/source :links/dest :links/type]))
                   (filter (comp some? :links/source))
                   (filter (comp some? :links/dest))
                   (filter #(= "\"id\"" (:links/type %)))
                   (map (fn [{:links/keys [source dest]}]
                          {:source (remove-quotes source)
                           :target (remove-quotes dest)}))
                   ;; need to do this bc there's some links out to nodes that were deleted
                   (filter #(contains? nodes (:source %)))
                   (filter #(contains? nodes (:target %)))
                   (into #{}))
        id->val (reduce
                 (fn [id->val _]
                   (power-law-step links id->val))
                 (into {} (map (fn [[k v]] [k 1])) nodes)
                 (range 2))
        nodes   (into {}
                      (map (fn [[k {:nodes/keys [file] :as v}]]
                             [k
                              (assoc v
                                     :nodes/created-time (roam-file-name->time-string file)
                                     :nodes/value (id->val k)
                                     :nodes/content (slurp file))]))
                      nodes)]
    [nodes links]))

(defn write-datascript-db!
  "writes datascript db to specified output-path. w/ attributes
  :node/{created|value|content|title|id}
  :link/{source|target}
  This will be used by the UI to make the site"
  [output-path]
  (let [[nodes links] (->nodes+links)]
    (spit output-path
          (-> (d/empty-db)
              (d/db-with
               (into []
                     (map #(select-keys (second %)
                                        [:nodes/content
                                         :nodes/id
                                         :nodes/title
                                         :nodes/value
                                         :nodes/properties]))
                     nodes))
              (d/db-with
               (into []
                     (map (fn [v]
                            (set/rename-keys v
                                             {:source :link/source
                                              :target :link/target})))
                     links))
              (pr-str)))))


(defmacro defnodes+links []
  (let [[nodes links] (->nodes+links)]
    `(do
       (def ~'nodes ~nodes)
       (def ~'links ~links))))

(defmacro def-node-title->content []
  (let [[nodes _links] (->nodes+links)
        title->content (into {}
                             (map (fn [[_ {:keys [title file]}]]
                                    [title (slurp file)]))
                             nodes)]
    `(def ~'node-title->content ~title->content)))


(comment

  ;; link/from link/to
  (let [schema {:node/id {:db/unique :db.unique/identity}}
        conn (d/create-conn schema)]
    (d/entity @conn [:db/ident :node/id]))

  (tap> (second (->nodes+links)))

  (require '[clojure.set :as set])

  (set/rename-keys {:a 'b} {:a :c})




  (filter even? [1 2 3])

  (some #(when (and (:links/dest %)
                    (= (remove-quotes (:links/dest %))
                       "dc4f2b75-b7e5-4db6-ba2c-c1bf01a42453"))
           %)
        (all-links-with-source-nodes jdbc-url))



  (def-node-title->content)

  (->> node-title->content
       (map (fn [[title content]]
              (org-parser/parse content)))
       (take 1)
       tap>)



  (let [ds (jdbc/get-datasource {:jdbcUrl jdbc-url})]
    (->> (jdbc/execute! ds ["
with
usable_nodes
as (select * from nodes
    where not exists (
        select * from links
        inner join nodes as dest ON dest.id=links.dest
        where -- source is node specified
        links.source=nodes.id and
        (
        -- dest is anything ekata or private...
        INSTR(dest.title, 'ekata') > 0
        or dest.title = '\"private\"'
        )
    )
    and INSTR(nodes.title, 'ekata') <= 0
)
select * from usable_nodes
join files on files.file = usable_nodes.file
where id = '\"13a39de3-44c1-4116-bb39-cbaac1768828\"'
"])
         (map (fn [{props :nodes/properties :as r}]
                (assoc r :props
                       (->> props
                            read-string
                            (map (fn [[k _dot v]]
                                   [k v]))
                            (into {})))))
         (map #(update % :files/mtime
                       (fn [x]
                         (let [[high low _ _] (read-string x)]
                           (java.time.Instant/ofEpochSecond
                            (+ (* high (Math/pow 2 16))
                               low))))))))


  ;; does the whole shebang from the db
  (write-datascript-db! "./public/db.edn")

  )
