(ns kev.roam.node-frontend
  (:require
   [reitit.frontend.easy :as rfe]
   [datascript.core :as d]
   [clojure.string :as str]
   [org-parser.parser :as org-parser]
   [re-frame.loggers :refer [console]]
   [kev.roam.data :as kev.data]
   [kev.roam.search :as kev.search]
   [cljs.core.match :refer-macros [match]]
   ["@mui/material" :as mui])
  (:require-macros
   [kev.roam.data-import :as data-import]))

(comment

  (prn "what")


  (:node/title
                  (kev.roam.data/find-node-by
                   kev.roam.data/db
                   :node/id "b77d4578-bc50-42a8-94c4-b11d27f78837"))

  (let [link [:link
              [:link-ext
               [:link-ext-id "b77d4578-bc50-42a8-94c4-b11d27f78837"]]]]
    (match link
    [:link [:link-ext link-ext]]
    (match link-ext
      [:link-ext-id id]
      (rfe/href :route/node
                {:title
                 (:node/title
                  (kev.roam.data/find-node-by
                   kev.roam.data/db
                   :node/id id))})
      :else  ::unknown)))

  )



(defn auto-id [s]
  (map-indexed (fn [idx item]
                 (with-meta item {:key idx}))
               s))

(defn query-backlinks [db node-title]
  (d/q
   '[:find [?backlink-title ...]
    :in $ ?node-title
    :where
    [?node :node/title ?node-title]
    [?node :node/id ?node-id]
    [?e :link/target ?node-id]
    [?e :link/source ?backlink-id]
    [?backlink-node :node/id ?backlink-id]
    [?backlink-node :node/title ?backlink-title]]
  db
  node-title))

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
   [
    {:name :section-header
     :re #"\n((\*+)\s(.*))\n"
     :find-idx 1
     :find-fn (juxt #(nth % 2) #(nth % 3))}

    ;; these three must be above :bullet
    {:name :underline
     :re #"\s(_(\S(.|\n)*?\S)_)\s"
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
     :re #":PROPERTIES:"}
    {:name :newline
     :re #"\n"}
    ]))

(tokenize-org
"
Here is a sample with _underlining text_ and _more_ with =code= and also /with
italics and/
"
 )

(re-find #"\s(\*(\S(.|\n)*\S)\*)\s"
"
Here is a sample with _underlining text_ and _more_ with =code= and also /with *italics
and* something
"
         )

(re-token
 "taco
    with commentary
** title too
"
 {:name :section-header
     :re #"\n((\*+)\s(.*))\n"
     :find-idx 1
     :find-fn (juxt #(nth % 2) #(nth % 3))})

(defn kev-link->href [link]
  (if-let [[_ id] (re-matches #"^id:(\S+)$" link)]
    (rfe/href :route/node
              {:title
               (:node/title
                (kev.roam.data/find-node-by
                 kev.roam.data/db
                 :node/id id))})
    link))

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

(defn filter-linebreaks
  "removes linebreaks where text should be grouped. This prevents weirdness.

  Combines when previous line when:
  - this line is not bulleted
  - this line has same indent as previous line, plus pervious line's bullet
  - previous line contains more than 50 characters

  Also! remove lead space if it's there when combining!!
  "
  [tokenized]
  (loop [[h & t]  tokenized
         filtered []]
    (match h
      [:newline _]
      (let [[rprev _]    (split-tokens-newline (reverse filtered))
            prevline     (reverse rprev)
            [nextline _] (split-tokens-newline t)
            [p-ind p-bull?] (indent-and-bullet prevline)
            [next-indent next-bull?] (indent-and-bullet nextline)
            prev-indent  (+ (if p-bull? 2 0) p-ind)]
        (if (and (< 60 (total-string-count prevline))
                 (not next-bull?)
                 (= prev-indent next-indent))
          (if (match (first t) [:lead-space _] true :else false)
            (recur (cons " " (rest t)) filtered)
            (recur t filtered))
          (recur t (conj filtered h))))

      nil filtered

      ;; join string to prev string
      ;; needed for spaces. idk if it makes sense here or if should be up the stack
      (s :guard #(and (string? %)
                      (string? (last filtered))))
      (recur t (assoc filtered (dec (count filtered))
                      (str (last filtered) " " s)))

      :else (recur t (conj filtered h)))))

(defn tokenized->hiccup [tokenized]
  (loop [[h & t] tokenized
         hiccup  []]
    (cond
      (and (empty? hiccup)
           (nil? h) (nil? t)) [:br]
      (nil? h) [:div (auto-id hiccup)]
      :else
      (match h
        [:lead-space indent-count]
        (let [[this-line remainder] (split-tokens-newline t)]
          (recur (rest remainder) ;; remove the newline, cuz this box creates a break
                 (conj hiccup
                       [:> mui/Box {:sx {:ml (str indent-count "ch")}}
                        (tokenized->hiccup this-line)])))

        [:bullet [indent bullet]]
        (let [[this-line remainder] (split-tokens-newline t)]
          (recur (rest remainder) ;; remove newline
                 (conj hiccup
                       [:> mui/Box {:sx {:ml (str (+ 2 indent) "ch")}}
                        [:> mui/Box {:sx {:ml "-2ch" :position "absolute"}} bullet]
                        (tokenized->hiccup this-line)])))

        [:underline text] (recur t (conj hiccup [:u text]))
        [:bold text] (recur t (conj hiccup [:b text]))
        [:italic text] (recur t (conj hiccup [:em text]))

        [:link link]
        (recur t (-> hiccup (conj [:a {:href link} link]) (conj [:span " "])))

        [:named-link [link text]]
        (recur t (conj hiccup [:a {:href (kev-link->href link)} text]))

        ;; maybe not do newline if indentation matches? idk
        [:newline _]
        (recur t (conj hiccup [:br]))

        [:section-header [stars text]]
        (recur t (conj hiccup [(case stars
                                 "*" :h1
                                 "**" :h2
                                 "***" :h3
                                 "****" :h4
                                 "*****" :h5) text]))

        [:props-start _] (let [[_ [_ & no-props-tail]] (split-with #(match % [:title-line _] false :else true)
                                                                   t)]
                           (recur no-props-tail hiccup))
        [:title-line _] (recur t hiccup)

        [& s] (recur t (conj hiccup [:span (pr-str h)]))

        :else
        (recur t (conj hiccup [:span h]))))))

(defn org->hiccup [text]
  [:div (->> text
             tokenize-org
             filter-linebreaks
             tokenized->hiccup
             ;(#(with-out-str (cljs.pprint/pprint %)))
             )])

(defn node-page
  "looks up the node by title (bc that's what we're using as the path parameter)
  and renders it"
  [db node-title]
  [:div
   [kev.search/search-box
    (->> (d/q '[:find [[pull ?e [:node/title :node/id :node/content]] ...]
              :where [?e :node/title ?title]
              [?e :node/content ?content]]
              db)
       (map (fn [{:node/keys [title content]}]
              {:title title
               :text content
               :on-select (fn [_]
                            (console :log "selected" title)
                            (rfe/push-state :route/node {:title title}))})))]
   [:> mui/Box {:sx {:display "flex"
                     :justify-content "space-between"}}
    [:> mui/Box
     {:sx {:display "flex" :flex-direction "column"}}
     [:h1 node-title]
     #_[:a {:href (rfe/href :route/home)} "home"]]
    [:> mui/Box
     {:sx {:display "flex"
           :flex-direction "column"
           :background-color "grey.200"
           :max-width "50%"
           :p 1
           ;:justify-content "space-around"
           :height "100%"}
      :component "span"}
     [:b "Backlinks:"]
     (->> (query-backlinks db node-title)
          (filter #(not= "home" %))
          (into ["home"])
          (map
           (fn [backlink-title]
             [:a {:href (rfe/href :route/node {:title backlink-title})}
              backlink-title]))
          (auto-id))]]
   (some->> (kev.roam.data/find-node-by db :node/title node-title)
            :node/content
            org->hiccup)])
