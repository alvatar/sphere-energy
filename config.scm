(sphere: "energy")
(dependencies:
 (condition
  (load (fabric: algorithm/list)))
 (filesystem
  (include (core: base-macros))
  (load (= log)))
 (functional-arguments
  (include (core: base-macros)))
 (remote/debuggee
  (include (core: base-macros))
  (load (= remote/rdi)))
 (repl-server
  (include (core: base-macros)))
 (rest-values
  (load (fabric: algorithm/list)))
 (template
  (include (core: base-macros)))
 (testing-macros
  (include (core: base-macros)
           (= condition-macros)))
 (testing
  (load (= condition))))
