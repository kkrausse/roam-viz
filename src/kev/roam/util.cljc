(ns kev.roam.util)

(def slet-data* (atom {}))

(defmacro slet
  "remembers results of previous evaluations"
  [kw bindings & forms]
  (let [bindings'
        (->> bindings
             (partition 2)
             (mapcat (fn [[sym form]]
                       [sym `(or (get-in @slet-data* [~kw ~(hash form)])
                                 (let [v# ~form]
                                   (swap! slet-data*
                                          assoc-in
                                          [~kw ~(hash form)]
                                          v#)
                                   v#))])))]
    `(let [~@bindings']
       ~@forms)))

(defmacro conda [test exp & forms]
  `(if-let [~'it ~test]
     ~exp
     ~(if (seq forms)
        `(conda ~@forms)
        nil)))

(defmacro js-fn [arglist & forms]
  `(fn [& args#]
     (let [~arglist (~'js->clj args# :keywordize-keys true)]
       ~@forms)))
