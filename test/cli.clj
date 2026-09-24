(ns cli)


(def invalid-tree
  {"git" {:cmd {"push" {:exec-fn println}
                "invalid-clone" {:exec-fn 1}}}})

(def invalid-tree-single-branch
  {"git" {:cmd {"invalid-clone" {:exec-fn 1}}}})
 
(def valid-tree
  {"git" {:cmd {"push" {:exec-fn println}
                "clone" {:exec-fn println}}}})