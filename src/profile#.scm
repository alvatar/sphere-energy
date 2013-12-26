;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Basic profiling

;;; Explicit definition

(define-macro (define-timed name+args #!rest body)
  (if (list? name+args)
      `(define ,name+args
         (%%accum-time ',(car name+args)
                       (lambda ()
                         ,@body)))
      `(define ,name+args ,@body)))
