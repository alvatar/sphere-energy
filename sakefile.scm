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
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m compiler-options: '(debug)))) modules)
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m))) modules))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task install ()
  ;; Install compiled module files
  (for-each (lambda (m) (sake#install-compiled-module m versions: '(() (debug)))) modules)
  (sake#install-sphere-to-system))

(define-task uninstall ()
  (sake#uninstall-sphere-from-system))

(define-task all (compile install)
  'all)
