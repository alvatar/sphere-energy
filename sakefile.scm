(define modules
  '(arguments
    error-code
    exception
    localization
    filesystem
    functional-arguments
    io
    log
    profile
    remote/debuggee
    repl-server
    rest-values
    shared-structure
    template
    testing
    time))

(define-task compile ()
  (unless (file-exists? (current-build-directory))
          (create-directory (current-build-directory)))
  ;; Copy internal file to build directory
  (copy-file (string-append (current-source-directory) "/remote/rdi.scm")
             (string-append (current-build-directory) "/rdi.scm"))
  ;; Make Sense script standalone
  (sake#expand-includes (string-append (current-source-directory) "remote/debug-server.scm")
                        (string-append (current-build-directory) "sense.scm"))
  ;; Compile all modules
  (sake#parallel-for-each
   (lambda (m)
     (sake#compile-module m cond-expand-features: '(debug) version: '(debug))
     (sake#compile-module m cond-expand-features: '(optimize)))
   modules))

(define-task post-compile ()
  (for-each (lambda (m) (sake#make-module-available m versions: '(() (debug)))) modules))

(define-task install ()
  ;; Install Sake extension
  (copy-file (string-append (current-build-directory) "sense.scm")
             "~~bin/sense")
  (sake#install-sphere-to-system))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)
