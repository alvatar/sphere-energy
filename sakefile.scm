(define modules
  '(arguments
    error-code
    condition
    localization
    filesystem
    functional-arguments
    io
    log
    profile
    remote/debuggee
    remote/rdi
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
  ;; Make debug server script standalone (expand includes)
  (sake#expand-includes (string-append (current-source-directory) "remote/debug-server.scm")
                        (string-append (current-build-directory) "debug-server.scm"))
  ;; Compile all modules
  (sake#parallel-for-each
   (lambda (m)
     (sake#compile-module m cond-expand-features: '(debug) version: '(debug))
     (sake#compile-module m cond-expand-features: '(optimize)))
   modules))

(define-task post-compile ()
  (for-each (lambda (m) (sake#make-module-available m versions: '(()))) modules))

(define-task install ()
  ;; Install Sense
  (copy-file (string-append (current-build-directory) "debug-server.scm")
             "~~bin/debug-server.scm")
  (copy-file (string-append (current-source-directory) "remote/sense")
             "~~bin/sense")
  (copy-file (string-append (current-source-directory) "remote/pump.scm")
             "~~bin/pump.scm")
  (copy-file (string-append (current-source-directory) "remote/sense-pump")
             "~~bin/sense-pump")
  (sake#install-sphere-to-system))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)
