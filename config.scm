(sphere: "energy")
(dependencies:
 ((= exception)
  (include
   (energy: exception-macros))
  (load
   (algorithm: list)))
 ((= testing-macros)
  (include
   (energy: exception-macros)))
 ((= testing)
  (include
   (energy: testing-macros))))
