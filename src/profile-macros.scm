;;; Macros for profile module

;; (define-macro (define-timed name+args #!rest body)
;;   (if (list? name+args)
;;       `(define ,name+args
;;          (%%accum-time ',(car name+args)
;;                        (lambda ()
;;                          ,@body)))
;;       `(define ,name+args ,@body)))
