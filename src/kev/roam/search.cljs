(ns kev.roam.search
  (:require
   [re-frame.loggers :refer [console]]
   [promesa.core :as p]
   [reagent.core :as r]
   [clojure.string :as str]
   [net.cgrand.xforms :as x]
   [cljs.core.match :refer-macros [match]]
   ["@mui/material" :as mui]
   ["fuzzysort" :as fuzzysort]))

(defn ->child-seq [text->elem chs]
  (->> chs
       (map (fn [x]
              (if (string? x)
                (text->elem x)
                x)))
       (map-indexed (fn [i x]
                 (with-meta x {:key i})))))

(defn rand-string [len]
  (apply str (repeatedly len #(rand-int 10))))

(defn fuzzysort-result->hiccup [result]
  (let [separator (rand-string 10)
        separated (fuzzysort/highlight result separator separator)]
    (->> (str/split separated (js/RegExp separator))
         (partition 3 2 '("" ""))
         (mapcat (fn [[start highlighted rest]]
                   (list start
                         [:mark highlighted]
                         rest)))
         (dedupe)
         (filter #(match %
                         [:mark ""] false
                         :else true))
         (filter (complement empty?)))))

(defn partition-str [s n step]
  (let [len (count s)]
    (loop [cur 0
           strs  []]
      (if (>= cur len)
        strs
        (recur (+ cur step)
               (conj strs
                     (subs s cur (+ n cur))))))))

(defn ->search-targets [index]
  (into []
        (x/for [{:keys [title text] :as obj} _
                :let [n 50
                      step 20]
                [i tidbit] (map-indexed vector (partition-str text n step))]
          (merge
           obj
           {:title title
            :start (* i step)
            :raw   text
            :text  (fuzzysort/prepare tidbit)}))
        index))

(defn search-box
  "general search box
  index is a seq of maps w/ keys :title :text :on-select

  see `->search-targets` for how text gets chopped up and used smaller chunks to search over"
  [index]
  (r/with-let [popover-anchor (r/atom nil)
               search-results (r/atom nil)]
    (let [search-targets   (clj->js (->search-targets index))
          before-after-len 40]
      [:> mui/Box
       {:on-change (fn [e]
                     (swap! popover-anchor #(or % (.-target e)))
                     (let [search  (.. e -target -value)
                           results (->> (fuzzysort/go search
                                                      search-targets
                                                      (clj->js {:limit     500
                                                                :threshold -100
                                                                :allowTypo true
                                                                :scoreFn (fn [[title text & _]]
                                                                         ;; deduct from non-title match
                                                                           (Math/max (or (and title
                                                                                              (aget title "score"))
                                                                                         -1000)
                                                                                     (or (and text
                                                                                              (- (aget text "score") 50))
                                                                                         -1000)))
                                                                :keys      [:title :text]}))
                                        (map (fn [{title-result 0 text-result 1 :as result}]
                                               (let [{:keys [id title start raw] :as obj} (js->clj (aget result "obj")
                                                                                                   :keywordize-keys true)
                                                     highlight-text                       (fuzzysort-result->hiccup text-result)
                                                     text-len                             (->> highlight-text
                                                                                               count)]
                                                 (merge
                                                  obj
                                                  {:score (aget result "score")
                                                   :text-highlight
                                                   (concat
                                                    ["..." (subs raw (- start before-after-len) start)]
                                                    highlight-text
                                                    [(subs raw start (+ start text-len before-after-len)) "..."])
                                                   :title-highlight (or (seq (fuzzysort-result->hiccup title-result))
                                                                        title)}))))
                                        (group-by :title)
                                        (map (fn [[_ [best & _]]]
                                             ;(console :log "best: " best)
                                               best))
                                        (take 20)
                                      ;; re-sort bc group-by fudged it
                                        (sort-by (comp (partial * -1) :score)))]
                       (reset! search-results
                               results)))}
       [:> mui/TextField
        {:variant "standard"
         :size    "small"
         :label   "search"}]
       [:div
        [:> mui/Popover
         {:anchor-origin      {:vertical   "bottom"
                               :horizontal "center"}
          :transform-origin   {:vertical "top"
                               :horizontal "left"}
          :sx                 {:width "100%"
                               :max-height "90%"} ;; this bc it covers the search box!
          :id                 "simple-popover"
          :disable-auto-focus true
          :on-close           #(reset! popover-anchor nil)
          :anchor-el          @popover-anchor
          :open               (boolean (and @popover-anchor (seq @search-results)))}
         [:> mui/List
          {:sx {:width "100%"}}
          (map-indexed
           (fn [i {:keys [text-highlight title-highlight post-id on-select]}]
             ^{:key i}
             [:> mui/ListItemButton
              {:sx       {}
               :on-click (fn [_]
                           (reset! popover-anchor nil)
                           (on-select))}
              [:> mui/Box
               {:sx {:m 1}}
               [:> mui/Typography
                {:variant   "h5"
                 :component "div"}
                (->child-seq #(do [:span %])
                             title-highlight)]
               [:> mui/Typography
                {:gutter-bottom true
                 :component     "div"
                 :variant       "subtitle1"}
                (->child-seq #(do [:span %])
                             text-highlight)]]])
           @search-results)]]]])))
