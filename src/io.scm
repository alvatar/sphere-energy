;;; Copyright (c) 2013-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Basic I/O extensions

;; Dump all contents readable from a u8vector port.
;; target-port = u8vector-port = dump to this port another u8vector port.
;;               #f = dump to u8vector, returned as return value.
;; .author Mikael Möre
(define* (dump-u8vector-port source-port (target-port #f))
  (if (not target-port)
      (call-with-output-u8vector
       '()
       (lambda (target-port)
         (dump-u8vector-port source-port target-port)))

      (let* ((tmp-bufsize (* 50 1024))
             (tmp-buffer (make-u8vector tmp-bufsize)))
        (let loop ()
          (let ((n (read-subu8vector tmp-buffer 0 tmp-bufsize source-port)))
            (if (> n 0)
                (begin
                  (write-subu8vector tmp-buffer 0 n target-port)
                  (loop))))))))

(define (u8vector->file u8vector filename)
  (call-with-output-file
      `(path: ,filename)
    (lambda (port)
      (write-subu8vector u8vector 0 (u8vector-length u8vector) port))))

(define (file->u8vector filename)
  (let ((r (call-with-output-u8vector
            '()
            (lambda (write-port)
              (let ((buf (make-u8vector 10240)))
                (call-with-input-file `(path: ,filename)
                  (lambda (port)
                    (let loop ()
                      (let ((r (read-subu8vector buf 0 10240 port)))
                        (write-subu8vector buf 0 r write-port)
                        (if (< 0 r) (loop)))))))))))
    r))

(define* (write-u8vector vec (port #f))
  (write-subu8vector vec
                     0
                     (u8vector-length vec)
                     (or port (current-output-port))))
