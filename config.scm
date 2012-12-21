(sphere: "energy")
(dependencies:
 ((= exception)
  (include
   (energy: exception-macros))
  (load
   (algorithm: srfi-1-list)))
 ((= testing-macros)
  (include
   (energy: exception-macros)))
 ((= testing)
  (include
   (energy: testing-macros))))
