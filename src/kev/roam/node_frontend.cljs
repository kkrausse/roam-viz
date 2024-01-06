(ns kev.roam.node-frontend
  (:require
   [reitit.frontend.easy :as rfe]
   [datascript.core :as d]
   [clojure.string :as str]
   [re-frame.loggers :refer [console]]
   [kev.roam.data :as roam.data]
   [kev.roam.search :as roam.search]
   [cljs.core.match :refer-macros [match]]
   [reagent.dom]
   [reagent.core]
   ["@mui/material" :as mui]
   ["@mui/icons-material/ExpandMore" :default ExpandMoreIcon]
   ["@mui/material/styles" :refer [useTheme]]
   [reagent.core :as r])
  (:require-macros
   [kev.roam.data-import :as data-import]))

(def ^:const content-width 80)

(comment

  (:nodes/title
                  (kev.roam.data/find-node-by
                   kev.roam.data/db
                   :nodes/id "b77d4578-bc50-42a8-94c4-b11d27f78837"))

  (let [link [:link
              [:link-ext
               [:link-ext-id "b77d4578-bc50-42a8-94c4-b11d27f78837"]]]]
    (match link
    [:link [:link-ext link-ext]]
    (match link-ext
      [:link-ext-id id]
      (rfe/href :route/node
                {:title
                 (:nodes/title
                  (kev.roam.data/find-node-by
                   kev.roam.data/db
                   :nodes/id id))})
      :else  ::unknown)))
  )

(defn auto-id [s]
  (map-indexed (fn [idx item]
                 (with-meta item {:key idx}))
               s))

(defn deep-merge [& maps]
  (apply merge-with
         (fn [m1 m2]
           (cond
             (or (nil? m1)
                 (nil? m2))        (or m2 m1)
             (every? map? [m1 m2]) (deep-merge m1 m2)
             :else                 m2))
         maps))

(defn collapsable [& {:keys [summary
                             contents
                             default-collapsed?
                             root-props
                             summary-props]}]
  (let [theme (useTheme)]
    (r/with-let [collapsed* (r/atom (boolean default-collapsed?))]
      [:div
       [:div (deep-merge {:style {:background-color "grey.200"
                                  :display          :inline-block
                                  :padding-left (theme.spacing 2)
                                  :padding-right    (theme.spacing 1)}}
                         root-props)
        [:div {:style    {:display        "inline-flex"
                          :flex-direction "row"}}
         [:a {:on-click (fn [_e] (swap! collapsed* not))
              :style (merge {:margin-left (theme.spacing -2)
                             :align-self "center"
                             :cursor :pointer
                             :width      (theme.spacing 3)
                             :flex       "0 1 auto"}
                            (when @collapsed*
                              {:transform "rotate(-90deg)"}))}
          [:> ExpandMoreIcon]]
         [:div {:style {:flex       "0 1 auto"
                        :align-self "center"}}
          summary]]
        (when-not @collapsed*
          [:div
           contents])]])))

(defn org-id->subheading [id]
  (str "id_" id))

(defn org-id->href [db id]
  (let [top-node (roam.data/find-top-node db id)]
    (if (= id (:nodes/id top-node))
      (rfe/href :route/node
                {:title (:nodes/title top-node)})
      (rfe/href :route/subheading
                {:title (:nodes/title top-node)
                 :subheading (org-id->subheading id)}))))

(defn linkable-heading [& {:keys [db component id text props]}]
  (let [theme (useTheme)]
    [component (deep-merge
                {:style {:margin 0
                         :line-height 1.5}}
                (when id
                  {:id (org-id->subheading id)})
                props)
     text
     (when-let [href (org-id->href db id)]
       [:a {:href  href
            :style {:padding-left (theme.spacing 0.5)
                    :cursor       :pointer
                    :color        (aget (.. theme -palette -grey ) 300)}}
        "#"])]))

(defn query-backlinks [db node-attr node-val]
  (d/q
   '[:find [[pull ?backlink-node [:nodes/id :nodes/title]] ...]
     :in $ ?attr ?value
     :where
     [?node ?attr ?value]
     [?node :nodes/id ?node-id]
     [?e :link/target ?node-id]
     [?e :link/source ?backlink-id]
     [?backlink-node :nodes/id ?backlink-id]]
   db
   node-attr node-val))

(defn backlinks [db & {:keys [node-attr node-val]}]
  (let [backlinks (query-backlinks db node-attr node-val)
        theme (useTheme)]
    (when (seq backlinks)
      [collapsable
       :default-collapsed? true
       :root-props {:style {:background-color (aget (.. theme -palette -grey ) 200)}}
       :summary [:b "backlinks"]
       :contents
       [:div {:style {:display "inline-flex"
                      :flex-direction "column"
                      :mr 1}}
        (->> backlinks
             (map
              (fn [{:nodes/keys [title id]}]
                [:div
                 {:style {:flex "0 1 auto"}}
                 [:a {:href (org-id->href db id)}
                  title]]))
             (auto-id))]
       ])))

(defn re-token [text {:keys [re name find-idx find-fn]
                      :or   {find-fn identity}}]
  (loop [text    text
         ret-seq []]
    (let [find (re-find re text)]
      (if (nil? find)
        (cond-> ret-seq (not= "" text) (conj text))
        (let [to-replace    (get find (or find-idx 0))
              replace-start (str/index-of text to-replace)]
          (recur
           (subs text (+ (count to-replace) replace-start))
           (cond-> ret-seq
             (< 0 replace-start) (conj (subs text 0 replace-start))
             true                (conj [name ((or find-fn identity) find)]))))))))

(defn tokenize-org [text]
  (reduce
   (fn [parse {:keys [re name find-idx find-fn] :as re-spec}]
     (mapcat (fn [item]
               (if (string? item)
                 (re-token item
                           re-spec)
                 [item]))
             parse))
   [text]
   [{:name :section-header
     :re #"\n((\*+)\s(.*))\n"
     :find-idx 1
     :find-fn (juxt #(nth % 2) #(nth % 3))}

    ;; these three must be above :bullet
    {:name :underline
     :re #"\s(_(\S(.|\n)*?\S)_)[^a-zA-Z]"
     :find-idx 1
     :find-fn (comp second rest)}
    {:name :bold
     :re #"\s(\*(\S(.|\n)*?\S)\*)\s"
     :find-idx 1
     :find-fn (comp second rest)}
    {:name :italic
     :re #"\s(/(\S(.|\n)*?\S)/)\s"
     :find-idx 1
     :find-fn (comp second rest)}

    {:name :bullet ;; must be above lead-space
     :re #"\n(( *)(\*|-) ).+\n"
     :find-idx 1
     :find-fn (fn [[_ _ indent bullet]]
                [(count indent) bullet])}
    {:name :lead-space
     :re #"\n( +)\S"
     :find-idx 1
     :find-fn (comp count second)}
    {:name :named-link
     :re #"\[\[([^\s\]]+)\]\[([^\]]+)\]\]"
     :find-fn (juxt second (comp second rest))}
    {:name :link
     :re #"(http[s]?://\S*)(\s|\n)"
     :find-idx 1
     :find-fn second}
    {:name :title-line
     :re #"\n(#\+title: (.+)\n)"
     :find-idx 1
     :find-fn (comp second rest)}
    {:name :props-start
     :re #"(:PROPERTIES:)"}
    {:name :props-end
     :re #"(:END:)\n"}
    {:name :begin-block
     :re #"(#\+begin_(\S+)(?:\s(\S*))*?)\n"
     :find-fn (comp (partial into []) rest rest)
     :find-idx 0}
    {:name :end-block
     :re #"(#\+end_(\S+).*?)\n"
     :find-fn #(get % 2)
     :find-idx 1}
    {:name :newline
     :re #"\n"}
    ]))

(defn split-tokens-newline [ts]
  (split-with
   #(match %
      [:newline _] false
      :else true)
   ts))

(defn total-string-count [struct]
  (let [c* (atom 0)]
    (clojure.walk/postwalk
     (fn [x]
       (when (string? x)
         (swap! c* + (count x))))
     struct)
    @c*))

(defn indent-and-bullet [line]
  (letfn [(bulleted? [x]
            (when x
              (some #(and (str/starts-with? x %) %) #{"- " "* " "**"})))]
    (match (first line)
      [:bullet [indent bullet]] [indent bullet]
      [:lead-space indent] [indent (bulleted? (second line))]
      nil                  [-1 true]
      :else                [0 (bulleted? (first line))])))

;; hoping this isn't too terrible w/ lazy seqs
(defn trim-linebreaks [tokenized]
  (->> tokenized
       (drop-while #(-> % first (= :newline)))
       (reverse)
       (drop-while #(-> % first (= :newline)))
       (reverse)))

(defn group-blocks [tokenized]
  (loop [[h & t]   tokenized
         grouped   []]
    (match h
           [:props-start _]
           (let [[contents remaining]
                 (split-with (fn [thing]
                               (match thing
                                      [:props-end _] false
                                      :else true))
                             t)]
             (recur (rest remaining)
                    (conj grouped
                          [:props {:org-props (->> contents
                                               (filter string?)
                                               (map (fn [s]
                                                      (->> s
                                                          (clojure.string/trim)
                                                          (re-matches #":(\S+): +(\S+)$")
                                                          (drop 1)
                                                          (into []))))
                                               (filter #(= 2 (count %)))
                                               (into {}))}])))

           [:section-header [stars text]]
           (let [[contents remaining] (split-with
                                       (fn [thing]
                                         (match thing
                                                [:section-header [content-stars _]]
                                                (> (count content-stars) (count stars))
                                                :else true))
                                       t)
                 contents (->> contents
                               group-blocks
                               trim-linebreaks)]
             (recur remaining
                    (conj grouped
                          [:section (merge
                                     {:stars stars
                                      :text text
                                      :org-props nil
                                      ;; NOTE: ignore newlines at beginning
                                      :contents contents}
                                     (when (-> contents first first (= :props))
                                       {:contents (->> contents
                                                       rest
                                                       (drop-while #(-> % first (= :newline))))
                                        :org-props (-> contents first second :org-props)}))])))
           [:begin-block [type & params]]
           (let [[new-t inner]
                 (loop [[h & t] t
                        inner []]
                   (match h
                          [:end-block type] [t inner]
                          nil   [t inner]
                          :else (recur t (conj inner h))))]
             (recur new-t (conj grouped [:block type params inner])))
           nil grouped
           :else (recur t (conj grouped h)))))

(comment
  (->> "
* ai topics
:PROPERTIES:
:ID:       776af6d0-9413-4156-b3e4-c5567e7e6b9c
:HAS_SUBNODES: 1
:END:

** Data Augmentation
:PROPERTIES:
:ID:       a46ace4f-7454-4ad2-9715-12d4aef9208d
:SUBNODE_OF: 776af6d0-9413-4156-b3e4-c5567e7e6b9c
:END:

"
       tokenize-org
       group-blocks
       )
  (comment
    [[:newline "\n"]
     [:section
      {:stars "*",
       :text "ai topics",
       :org-props
       {"ID" "776af6d0-9413-4156-b3e4-c5567e7e6b9c", "HAS_SUBNODES" "1"},
       :contents
       ([:section
         {:stars "**",
          :text "Data Augmentation",
          :org-props
          {"ID" "a46ace4f-7454-4ad2-9715-12d4aef9208d",
           "SUBNODE_OF" "776af6d0-9413-4156-b3e4-c5567e7e6b9c"},
          :contents ()}])}]])
  )

(defn tokenized->hiccup [parsed-org & {:keys [db] :as opts}]
  (loop [[h & t] parsed-org
         hiccup  []]
    (cond
      (and (empty? hiccup)
           (nil? h) (nil? t)) nil
      (nil? h)                (auto-id hiccup)
      :else
      (match h
             [:lead-space indent-count]
             (let [[this-line remainder] (split-tokens-newline t)]
               (recur (rest remainder) ;; remove the newline, cuz this box creates a break
                      (conj hiccup
                            [:div {:style {:margin-left (str indent-count "ch")}}
                             (tokenized->hiccup this-line opts)])))

             [:bullet [indent bullet]]
             (let [[this-line remainder] (split-tokens-newline t)]
               (recur (rest remainder) ;; remove newline
                      (conj hiccup
                            [:div {:style {:margin-left (str (+ 2 indent) "ch")}}
                             [:span {:style {:margin-left "-2ch"
                                             :position "absolute"}}
                              bullet]
                             (tokenized->hiccup this-line opts)])))

             [:underline text] (recur t (conj hiccup [:u text]))
             [:bold text] (recur t (conj hiccup [:b text]))
             [:italic text] (recur t (conj hiccup [:em text]))

             [:link link]
             (recur t (-> hiccup (conj [:a {:href link} link]) (conj [:span " "])))

             [:named-link [link text]]
             (recur t (conj hiccup
                            [:a {:href (if-let [[_ id] (re-matches #"^id:(\S+)$" link)]
                                         (org-id->href db id)
                                         link)}
                             text]))

             ;; maybe not do newline if indentation matches? idk
             [:newline _]
             (recur t (conj hiccup [:br]))

             [:section {:stars     stars
                        :text      text
                        :org-props props
                        :contents  contents}]
             (recur t
                    (conj hiccup
                          [collapsable
                           :summary [linkable-heading
                                     :db db
                                     :component (case stars
                                                  "*"     :h1
                                                  "**"    :h2
                                                  "***"   :h3
                                                  "****"  :h4
                                                  "*****" :h5)
                                     ;;:props {:style {:font-size "1.4em"}}
                                     :id (get props "ID")
                                     :title text
                                     :text text]
                           :contents [:<>
                                      (when-let [id (get props "ID")]
                                        [:<>
                                         [backlinks db
                                          :node-attr :nodes/id
                                          :node-val id]])
                                      (tokenized->hiccup contents opts)]]))

             [:props-start _] (recur (->> t
                                          (drop-while
                                           #(match %
                                                   [:props-end _] false
                                                   :else  true))
                                          (drop 1))
                                     hiccup)
             [:title-line _] (recur t hiccup)
             [:props _] (recur t hiccup)

             [:block type params content]
             (recur t (conj hiccup
                            [:> mui/Box
                             {:sx {:background-color "grey.300"
                                   :padding-left     2}}
                             [:> mui/Box
                              {:sx {:background-color "grey.200"
                                    :padding          1}}
                              [:em (tokenized->hiccup content opts)]]]))

             [& s] (recur t (conj hiccup [:span (pr-str h)]))

             :else
             (recur t (conj hiccup [:span h]))))))

(defn org->hiccup [org-text & {:keys [_db] :as opts}]
  [:<>
   (-> org-text
       (tokenize-org)
       (group-blocks)
       (tokenized->hiccup opts))])

(defn node-page
  "looks up the node by title (bc that's what we're using as the path parameter)
  and renders it"
  [db & {:keys [node-title subheading]}]
  (let [theme (useTheme)]
    [:div
     {:style {:max-width (.spacing theme content-width)
              :width     "100%"}
      :ref (fn [_el]
             (when-let [subheading-el (and subheading
                                           (seq subheading)
                                           (.querySelector js/document (str "#" subheading)))]
               (.scrollIntoView subheading-el)))}
     (when (not= node-title "home")
       [:a {:href (rfe/href :route/home)} "home"])
     [linkable-heading
      :db db
      :component :h1
      :title node-title
      :id (:nodes/id (roam.data/find-node-by db :nodes/title node-title))
      :text node-title]
     [backlinks db
      :node-attr :nodes/title
      :node-val node-title]
     [org->hiccup
      (some->> (kev.roam.data/find-node-by db :nodes/title node-title)
               :nodes/content)
      :db db]]))

(comment

  (let [subheading "9a9469fb-8ed0-4236-a039-2b7e3cb011a2"]
    (-> js/document
        (.querySelector (str "#id_" subheading))
       ; (.scrollIntoView)
        ))
  (->> (d/q '[:find [[pull ?e [*]]]
                :where [?e :nodes/title ?title]
                [?e :nodes/content ?content]]
              kdb)
         (map (fn [{:nodes/keys [title content]}]
                {:title title
                 :text content
                 :on-select (fn [_]
                              (console :log "selected" title)
                              (rfe/push-state :route/node {:title title}))})))
  )
