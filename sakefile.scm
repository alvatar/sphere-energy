(define modules
  '(arguments
    debug/debuggee
    error-code
    exception
    localization
    functional-arguments
    io
    profile
    repl-server
    rest-values
    shared-structure
    template
    testing
    time))

(define-task compile ()
  (for-each (lambda (m)
              (sake#compile-module m cond-expand-features: '(debug) version: '(debug))
              (sake#compile-module m cond-expand-features: '(optimize)))
            modules))

(define-task post-compile ()
  (for-each (lambda (m) (sake#make-module-available m versions: '(() (debug)))) modules))

(define-task install ()
  (sake#install-sphere-to-system))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)
