(ns kev.roam.graph
  (:require
   [reagent.core]
   [re-frame.loggers :refer [console]]
   [reitit.frontend.easy :as rfe]
   [datascript.core :as d]
   [reagent.dom]
   ["d3" :as d3]
   [re-frame.db :as db])
  (:require-macros
   [kev.roam.util :as util]
   [kev.roam.data-import :as sqlite-import]))

(defn graph-simulation!
  "nodes of format [{:id 'x :title 'something-cool :value 23}]
   links of format [{:source 'x :target 'y}]
  where 'x and 'y are the same type"
  [{:keys [svg width height nodes links]}]
  (let [node-radius (fn [node]
                      (* 6 (:value node)))
        force-node (-> (d3/forceManyBody)
                       (.strength (util/js-fn [x]
                                              (* -1.2 (* (node-radius x)
                                                         (node-radius x))))))
        force-link (.. (d3/forceLink links)
                       (distance (constantly 50))
                       ;(strength (constantly 30))
                       (id (util/js-fn [{:keys [id] :as force-thing}] id)))
        svg        (-> (d3/select svg)
                       (.attr "viewBox" (clj->js [(* -1 (/ width 2))
                                                  (* -1 (/ height 2))
                                                  width
                                                  height]))
                       (.attr "x" 0)
                       (.attr "y" 0)
                       (.attr "style" (str "width: 100%; height: auto; height: intrinsic;")))
        defs       (.append svg "svg:defs")
        _end-arrow (-> defs
                       (.append "svg:marker")
                       (.attr "id" "end-arrow")
                       (.attr "viewBox" "0 -5 10 10")
                       (.attr "refX" 10)
                       (.attr "markerWidth" 10)
                       (.attr "markerHeight" 10)
                       (.attr "orient" "auto")
                       (.append "svg:path")
                       (.attr "fill-opacity" 0.7)
                       (.attr "d" "M0,-5L10,0L0,5"))
        link (-> svg
                 (.append "g")
                 (.attr "stroke" "#999")
                 (.attr "stroke-opacity" 0.7)
                 (.attr "stroke-width" 1)
                                        ;(.attr "stroke-linecap" "round")
                 (.style "marker-end" "url(#end-arrow)")
                 (.selectAll "line")
                 (.data links)
                 (.join "line")
                 )
        node (-> svg
                 (.append "g")
                 (.attr "fill-opacity" 1)
                 (.attr "fill" "cyan")
                 (.attr "stroke" "#ffff")
                 (.attr "stroke-opacity" 0.3)
                 (.attr "stroke-width" 1.5)
                 (.selectAll "circle")
                 (.data nodes)
                 (.join "g")
                 (.attr "text-anchor" "middle"))
        _    (-> node
                 (.append "circle")
                 (.attr "r" (util/js-fn [x]
                                        (node-radius x))))

        ;; add link and on-hover (darken & clickable mouse) to circle
        _    (-> node
                 ;(.append "a")
                 (.append "foreignObject")
                 (.attr "x" (util/js-fn [x] (- 0 (node-radius x))))
                 (.attr "y" (util/js-fn [x] (- 0 (node-radius x))))
                 (.attr "width" (util/js-fn [x] (* 2 (node-radius x))))
                 (.attr "height" (util/js-fn [x] (* 2 (node-radius x))))
                 (.attr "font-size" (util/js-fn [x]
                                                (+ 7 (/ (node-radius x)
                                                        4))))
                 (.attr "style" "overflow: visible;")
                 (.append "xhtml:div")
                 (.attr "width" "100%")
                 (.attr "height" "100%")
                 (.attr "style" "display: flex; justify-content: center; align-items: center; position: absolute; top: 0; right: 0; bottom: 0; left: 0;")
                 (.append "a")
                 (.attr "href" (util/js-fn [{:keys [id title]}]
                                           (rfe/href :route/node {:title title})))
                 (.attr "style" "flex: 0 0 100%; text-align: center;")
                 (.text (util/js-fn [{:keys [id title] :as d}]
                                    (apply str (take 30 title))))
                 (.attr "fill" "black")
                 (.attr "fill-opacity" 1)
                 (.attr "y" "0.4em")
                 )
        ->txy (fn [{:keys [source target]}]
                (let [{sx :x sy :y} source
                      {tx :x ty :y} target
                      dist (js/Math.sqrt
                            (+ (js/Math.pow (- sx tx) 2)
                               (js/Math.pow (- sy ty) 2)))
                      dx (/ (- tx sx) dist)
                      dy (/ (- ty sy) dist)]
                  {:x (- tx (* (node-radius target) dx))
                   :y (- ty (* (node-radius target) dy))}))
        tick     (fn []
                   (-> link
                       (.attr "x1" #(-> % .-source .-x))
                       (.attr "y1" #(-> % .-source .-y))
                       (.attr "x2" (util/js-fn [e]
                                               (:x (->txy e))))
                       (.attr "y2" (util/js-fn [e]
                                               (:y (->txy e)))))
                   (-> node
                       (.attr "transform" (util/js-fn [{:keys [x y]}]
                                                      (str "translate(" x "," y ")")))
                       (.attr "x" #(.-x %))
                       (.attr "y" #(.-y %))))
        simulation (-> (d3/forceSimulation nodes)
                       (.force "link" force-link)
                       (.force "charge" force-node)
                       (.force "x" (d3/forceX))
                       (.force "y" (d3/forceY))
                       (.on "tick" tick))
        _          (-> node
                       (.call (-> (d3/drag)
                                  (.on "start"
                                       (fn drag-start [e]
                                         (when (= 0 (.-active e))
                                           (.. simulation (alphaTarget 0.3) (restart)))
                                         (aset e "subject" "fx" (.. e -subject -x))
                                         (aset e "subject" "fy" (.. e -subject -y))))
                                  (.on "drag"
                                       (fn drag [e]
                                         (aset e "subject" "fx" (.. e -x))
                                         (aset e "subject" "fy" (.. e -y))))
                                  (.on "end"
                                       (fn drag-end [e]
                                         (when (= 0 (.-active e))
                                           (.. simulation (alphaTarget 0)))
                                         (aset e "subject" "fx" nil)
                                         (aset e "subject" "fy" nil))))))]
    svg))

(defn remove-ns-keys [m]
  (into {}
    (map (fn [[k v]]
           [(keyword (name k)) v]))
    m))

(comment
  (get nodes "a262f204-b467-4012-beeb-fca8a629d0aa")


  (->> links
       (filter (fn [{:keys [source target]}]
                 (or #_(not (contains? nodes source))
                  (not (contains? nodes target))))))

  (require '[kev.roam.data :refer [db]])

  (->> db
       (d/q '[:find [[pull ?e [*]] ...]
          :where [?e :link/source _]])
       (map remove-ns-keys)
       first)

  "(function kev$roam$graph$graph_simulation_BANG__$_taco(e){
cljs.core.prn.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" wut? "], 0));

return re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null," log "," log ",-1595516004),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" clicked ",e], 0));
})();"
  )

(defn node-graph [db]
  (reagent.core/create-class
   {:component-did-mount
    (fn [this]
      (let [links (->> db
                       (d/q '[:find [[pull ?e [*]] ...]
                              :where [?e :link/source _]])
                       (map remove-ns-keys))
            nodes (->> db
                       (d/q '[:find [[pull ?e [:nodes/title :nodes/id :nodes/value]] ...]
                          :where [?e :nodes/title _]])
                       (map remove-ns-keys))]
        (graph-simulation! {:svg (reagent.dom/dom-node this)
                            :width 2400 :height 1600
                            :nodes (clj->js nodes)
                            :links (clj->js links)})))
    :render (fn []
              [:svg {"xmlns:xhtml" "http://www.w3.org/1999/xhtml"
                     "xmlnsXlink" "http://www.w3.org/1999/xlink"
                     :version "1.1"
                     :xmlns "http://www.w3.org/2000/svg"
                     :style {:width "100%" :height "auto"}}])}))
