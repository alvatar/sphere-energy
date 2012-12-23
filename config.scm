(sphere: "energy")
(dependencies:
 (exception
  (load (algorithm: list)))
 (program-arguments
  (include (core: base-macros)))
 (testing-macros
  (include (= exception-macros)))
 (time
  (include (core: optionals-macros))))
