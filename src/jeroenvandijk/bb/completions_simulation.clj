(ns jeroenvandijk.bb.completions-simulation
  (:require
   [edamame.core :as edamame]
   [flatland.ordered.map :as m]))


(defn resolve-symbol [v]
  (if (requiring-resolve v)
    [:ok]
    [:error "Could not resolve " v]))


(defn map-like? [x]
  (and (sequential? x)
       (every? (fn [y]
                 (and (sequential? y)
                      (= (bounded-count 10 y) 2)))
               x)))


(def ^:dynamic *level* -1)



(defn build-cmd
  [x check-fn]
  (binding [*level* (inc *level*)]
    (if (or (map? x)
            (map-like? x))
      (let [[problem cmds]
            (reduce (fn [[problem acc] [cmd config]]
                      (let [[status :as ret] (check-fn config)]
                        [(or problem (not= status :ok))
                         (assoc acc cmd ret)]))
                    [false (m/ordered-map)]
                    x)]
        (if problem
          [:nested-problem "Problem in subcommand" cmds]

          [:ok nil cmds]))
      [:error (str ":cmd should be map or sequence of pairs, got " x)])))

(defn class-name [x]
  (.getName (class x)))

(defn check-fn-symbol [k v]
  (if (namespace v)
    (if-let [v (requiring-resolve v)]
      (if (and (var? v)
               (fn? @v))
        [:ok]
        [:error (str k " should resolve to a function, " v " is a " (class-name @v))])
      [:error (str v " in " k " does not resolve")])
    [:error (str k " should be a qualified symbol, " v " is not ")]))


(def ^:dynamic *resolved* false)


(defn check-fn-field [k config]
  (let [v (get config k)]
    (cond (fn? v)
          [:ok]

          (and (symbol? v)
               (not *resolved*))
          (check-fn-symbol k v)

          :else
          [:error (str k " should be a function, "  v " is a " (class-name v))])))


(defn load-requires [config]
  (when-let [requires (:requires config)]
    (doseq [r requires]
      (try
        (require r)
        (catch Exception _
          [:error "require '" (pr-str r) "failed"])))))


(defn check [config]
  (or (load-requires config)
      (cond (:fn config)
            (check-fn-field :fn config)

            (:exec-fn config)
            (check-fn-field :exec-fn config)

            (:cmd config)
            (if *resolved*
              (build-cmd (:cmd config) check)
              (if (symbol? (:cmd config))
                (binding [*resolved* true]
                  (if-let [v (requiring-resolve (:cmd config))]
                    (if (and (var? v)
                             (or (map? @v)
                                 (map-like? @v)))
                      (build-cmd @v check)
                      [:error (str v " is a " (class @v) " not a map")])
                    [:error (str (:cmd config) " does not resolve")]))
                (build-cmd (:cmd config) check)))

            (list? config)
            (if *resolved*
              [:error ":cmd needs to be a map"]
              [:ok])

            (symbol? config)
            (if *resolved*
              [:error ":cmd needs to be a map"]
              [:ok])

            :else
            ;; Forms allowed only at the top level
            (or
             (when (zero? *level*)
               (or
                (when-let [t (:task config)]
                  (cond (symbol? t) (check-fn-symbol :task t)
                        (list? t)
                        (if (empty? t)
                          [:error ":task cannot be an empty form"]
                          [:ok])

                        :else
                        [:error ":task should be a function symbol or a form"]))

                (when-let [e (:exec config)]
                  (check-fn-symbol :exec e))))

             [:incomplete (str "Missing config " config)]))))

(comment

  (check {:fn nil})
  (check {:exec-fn nil})
  (check {:cmd nil})
  (check {:cmd {}})

  (check {:cmd  {"git" {:cmd {"push" {:exec-fn println}
                              "invalid-clone" {:exec-fn 1}}}}})

  (check {:cmd  {"git" {:cmd {"invalid-clone" {:exec-fn 1}}}}})

  :-)


(defn red [s]
  (str "\u001b[31m" s "\u001b[0m"))


(defn green [s]
  (str "\u001b[32m" s "\u001b[0m"))

(defn yellow [s]
  (str "\u001b[33m" s "\u001b[0m"))


(defn valid-task [task]
  (str (green "✓") " " task))


(defn invalid-task [task msg]
  (str (red "X") " " task " => " msg))

(defn invalid-nested-task [task msg]
  (str (yellow "#") " " task " => " msg))


(defn analysis->tree [cmd [status problem nesting]]
  (cond-> {:doc (case status 
                  :ok (valid-task cmd)
                  :nested-problem (invalid-nested-task cmd problem)
                  (invalid-task cmd problem))}
    nesting
    (assoc :cmd
           (into (m/ordered-map) #_(empty nesting)
                 (map (fn [[cmd v]]                      
                        [cmd (analysis->tree cmd v)]))
                 nesting))))

(defn build-analysis [x]
  (build-cmd x check))

(defn build-tree [x]
  (analysis->tree nil (build-analysis x)))


(comment

  (analysis->tree nil [:ok nil nil])
  (analysis->tree nil [:error "Problem" nil])
  (analysis->tree nil [:error "Problem" {"a" [:ok "V"]
                                         "b" [:error " no qualified symbol"]
                                         "c" [:incomplete "missing task"]}])

  (analysis->tree nil [:ok nil
                       {"main"
                        [:error "Problem in subcommand"
                         {"a" [:ok "V"]
                          "b" [:error " no qualified symbol"]
                          "c" [:incomplete "missing task"]}]}])

  (def correct-example-resolved-tree
    {"main"
     {:doc "main - warning"
      :cmd {"a" {:doc "ok"
                 :exec-fn println}
            "b" {:doc "error: no qualified symbol"
                 :fn println}
            "c" {:doc "incomplete: missing task" :fn println}}}})

  (build-cmd correct-example-resolved-tree check)
  (build-tree correct-example-resolved-tree)

  (def wrong-example-resolved-tree
    {"aa" (get correct-example-resolved-tree "main")
     "main"
     {:doc "main - warning"
      :cmd {"a" {:doc "ok"
                 :exec-fn 1}
            "b" {:doc "error: no qualified symbol"
                 :fn 2}
            "c" {:doc "incomplete: missing task"}}}})
  
  (build-tree correct-example-resolved-tree)

  :-)

;; Have a default tree, otherwise the simulation shows up as an invalid task
(def tree {})

;; Only re-define tree when we load from babashka
(when (System/getProperty "babashka.version")
  (def tasks
    (-> (slurp (str (System/getenv "PWD") "/bb.edn"))
        (edamame/parse-string {:map m/ordered-map})
        :tasks))

  (def app-tree 
    (if-let [[_ problem] (load-requires tasks)]
      {"main" {:doc (invalid-task nil problem)}}
      (-> (build-tree (dissoc tasks :init :requires))
          :cmd
             
          )))

(defn print-tree [& args]
  (clojure.pprint/pprint app-tree))

(defn print-analysis [& args]
  (clojure.pprint/pprint (build-analysis (dissoc tasks :init :requires))))

(def tree
  (assoc app-tree 
         ::print-tree {:exec-fn print-tree}
         ::print-analysis {:exec-fn print-analysis})))
