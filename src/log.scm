;;; Copyright (c) 2014 by Álvaro Castro Castilla
;;; Logging utilities

;;! Generic log message
(define (log type . message)
  (display "*** ")
  (display type)
  (display " -- ")
  (for-each print message)
  (newline))

;;! Info message
(define (info . message)
  (apply log (cons "INFO" message)))

;;! Info message with color
(define (info/color color . message)
  (let ((color-string
         (case color
           ((black) "\033[00;30m")
           ((dark-gray) "\033[01;30m")
           ((blue) "\033[00;34m")
           ((light-blue) "\033[01;34m")
           ((green) "\033[00;32m")
           ((light-green) "\033[01;32m")
           ((cyan) "\033[00;36m")
           ((light-cyan) "\033[01;36m")
           ((red) "\033[00;31m")
           ((light-red) "\033[01;31m")
           ((purple) "\033[00;35m")
           ((light-purple) "\033[01;35m")
           ((brown) "\033[00;33m")
           ((yellow) "\033[01;33m")
           ((light-gray) "\033[00;37m")
           ((white) "\033[01;37m")
           (else ""))))
    (apply log (append `("INFO" ,color-string) message))
    (display "\033[00m")))

;;! Warn message
(define (warn . message)
  (display "\033[00;33m")
  (apply log (cons "WARNING" message))
  (display "\033[00m"))

;;! Error message
(define (err . message)
  (display "\033[00;31m")
  (apply log (cons "ERROR" message))
  (display "\033[00m")
  (error "error, aborting"))
