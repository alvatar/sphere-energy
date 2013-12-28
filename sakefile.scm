(define modules
  '(debug/debuggee
    error-code
    exception
    functional
    localization
    profile
    program-arguments
    repl-server
    rest-values
    shared-structure
    template
    testing
    time))

(define-task compile ()
  (copy-file (string-append (current-source-directory) "debug/rdi.scm")
             (string-append (current-build-directory) "rdi.scm"))
  (for-each (lambda (m) (sake#compile-module m compiler-options: '(debug))) modules)
  (for-each sake#compile-module modules))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task install ()
  ;; Install compiled module files
  (for-each (lambda (m) (sake#install-compiled-module m versions: '(() (debug)))) modules))

(define-task force-install ()
  (sake#install-sphere-to-system))

(define-task all (compile install)
  'all)
