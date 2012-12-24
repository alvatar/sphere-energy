(sphere: "energy")
(dependencies:
 (exception
  (load (algorithm: list)))
 (program-arguments
  (include (core: base-macros)))
 (testing-macros
  (include (= exception-macros)))
 (testing
  (load (energy: exception)))
 (time
  (include (core: optionals-macros))))
