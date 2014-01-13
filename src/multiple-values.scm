;;; Copyright (c) 2014, Alvaro Castro-Castilla. All rights reserved.
;;; Syntax helpers for dealing with multiple values


;;! SRFI-71
;; Reference implementation of SRFI-71 (srfi-let/*/rec)
;; Sebastian.Egner@philips.com, 20-May-2005, PLT 208
;; Modified by Álvaro Castro-Castilla
;; Macros used internally are named i:<something>.
;; Abbreviations for macro arguments:
;;   bs  - <binding spec>
;;   b   - component of a binding spec (values, <variable>, or <expression>)
;;   v   - <variable>
;;   vr  - <variable> for rest list
;;   x   - <expression>
;;   t   - newly introduced temporary variable
;;   vx  - (<variable> <expression>)
;;   rec - flag if letrec is produced (and not let)
;;   cwv - call-with-value skeleton of the form (x formals)
;;         (call-with-values (lambda () x) (lambda formals /payload/))
;;         where /payload/ is of the form (let (vx ...) body1 body ...).
;; Remark (*):
;;   We bind the variables of a letrec to i:undefined since there is
;;   no portable (R5RS) way of binding a variable to a values that
;;   raises an error when read uninitialized.

(define-syntax srfi-letrec*             ; -> srfi-letrec
  (syntax-rules ()
    ((srfi-letrec* () body1 body ...)
     (srfi-letrec () body1 body ...))
    ((srfi-letrec* (bs) body1 body ...)
     (srfi-letrec (bs) body1 body ...))
    ((srfi-letrec* (bs1 bs2 bs ...) body1 body ...)
     (srfi-letrec (bs1) (srfi-letrec* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-letrec              ; -> i:let
  (syntax-rules ()
    ((srfi-letrec ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #t () () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax srfi-let* ; -> srfi-let
  (syntax-rules ()
    ((srfi-let* () body1 body ...)
     (srfi-let () body1 body ...))
    ((srfi-let* (bs) body1 body ...)
     (srfi-let (bs) body1 body ...))
    ((srfi-let* (bs1 bs2 bs ...) body1 body ...)
     (srfi-let (bs1) (srfi-let* (bs2 bs ...) body1 body ...)))))

(define-syntax srfi-let                 ; -> i:let or i:named-let
  (syntax-rules ()
    ((srfi-let ((b1 b2 b ...) ...) body1 body ...)
     (i:let "bs" #f () () (body1 body ...) ((b1 b2 b ...) ...)))
    ((srfi-let tag ((b1 b2 b ...) ...) body1 body ...)
     (i:named-let tag () (body1 body ...) ((b1 b2 b ...) ...)))))

(define-syntax i:let
  (let-syntax ((r5rs-let let)
               (r5rs-letrec letrec))
    (syntax-rules (values)
      ;; (i:let "bs" rec (cwv ...) (vx ...) body (bs ...))
      ;;   processes the binding specs bs ... by adding call-with-values
      ;;   skeletons to cwv ... and bindings to vx ..., and afterwards
      ;;   wrapping the skeletons around the payload (let (vx ...) . body).
      ;; no more bs to process -> wrap call-with-values skeletons
      ((i:let "bs" rec (cwv ...) vxs body ())
       (i:let "wrap" rec vxs body cwv ...))
      ;; recognize form1 without variable -> dummy binding for side-effects
      ((i:let "bs" rec cwvs (vx ...) body (((values) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (dummy (begin x #f))) body (bs ...)))
      ;; recognize form1 with single variable -> just extend vx ...
      ((i:let "bs" rec cwvs (vx ...) body (((values v) x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; recognize form1 without rest arg -> generate cwv
      ((i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...))
       (i:let "form1" rec cwvs vxs body (bs ...) (x ()) (values v ...)))
      ;; recognize form1 with rest arg -> generate cwv
      ((i:let "bs" rec cwvs vxs body (((values . vs) x) bs ...))
       (i:let "form1+" rec cwvs vxs body (bs ...) (x ()) (values . vs)))
      ;; recognize form2 with single variable -> just extend vx ...
      ((i:let "bs" rec cwvs (vx ...) body ((v x) bs ...))
       (i:let "bs" rec cwvs (vx ... (v x)) body (bs ...)))
      ;; recognize form2 with >=2 variables -> transform to form1
      ((i:let "bs" rec cwvs vxs body ((b1 b2 b3 b ...) bs ...))
       (i:let "form2" rec cwvs vxs body (bs ...) (b1 b2) (b3 b ...)))
      ;; (i:let "form1" rec cwvs vxs body bss (x (t ...)) (values v1 v2 v ...))
      ;;   processes the variables in v1 v2 v ... adding them to (t ...)
      ;;   and producing a cwv when finished. There is not rest argument.
      ((i:let "form1" rec (cwv ...) vxs body bss (x ts) (values))
       (i:let "bs" rec (cwv ... (x ts)) vxs body bss))
      ((i:let "form1" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v ...))
       (i:let "form1" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v ...)))
      ;; (i:let "form1+" rec cwvs vxs body bss (x (t ...)) (values v ... . vr))
      ;;   processes the variables in v ... . vr adding them to (t ...)
      ;;   and producing a cwv when finished. The rest arg is vr.
      ((i:let "form1+" rec cwvs (vx ...) body bss (x (t ...)) (values v1 v2 . vs))
       (i:let "form1+" rec cwvs (vx ... (v1 t1)) body bss (x (t ... t1)) (values v2 . vs)))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x (t ...)) (values v1 . vr))
       (i:let "bs" rec (cwv ... (x (t ... t1 . tr))) (vx ... (v1 t1) (vr tr)) body bss))
      ((i:let "form1+" rec (cwv ...) (vx ...) body bss (x ()) (values . vr))
       (i:let "bs" rec (cwv ... (x tr)) (vx ... (vr tr)) body bss))
      ;; (i:let "form2" rec cwvs vxs body bss (v ...) (b ... x))
      ;;   processes the binding items (b ... x) from form2 as in
      ;;   (v ... b ... x) into ((values v ... b ...) x), i.e. form1.
      ;;   Then call "bs" recursively.
      ((i:let "form2" rec cwvs vxs body (bs ...) (v ...) (x))
       (i:let "bs" rec cwvs vxs body (((values v ...) x) bs ...)))
      ((i:let "form2" rec cwvs vxs body bss (v ...) (b1 b2 b ...))
       (i:let "form2" rec cwvs vxs body bss (v ... b1) (b2 b ...)))
      ;; (i:let "wrap" rec ((v x) ...) (body ...) cwv ...)
      ;;   wraps cwv ... around the payload generating the actual code.
      ;;   For letrec this is of course different than for let.
      ((i:let "wrap" #f vxs body)
       (r5rs-let vxs . body))
      ((i:let "wrap" #f vxs body (x formals) cwv ...)
       (call-with-values
           (lambda () x)
         (lambda formals (i:let "wrap" #f vxs body cwv ...))))
      ((i:let "wrap" #t vxs body)
       (r5rs-letrec vxs . body))
      ((i:let "wrap" #t ((v t) ...) body cwv ...)
       (r5rs-let ((v 'undefined) ...)  ; (*)
                 (i:let "wraprec" ((v t) ...) body cwv ...)))
      ;; (i:let "wraprec" ((v t) ...) body cwv ...)
      ;;   generate the inner code for a letrec. The variables v ...
      ;;   are the user-visible variables (bound outside), and t ... 
      ;;   are the temporary variables bound by the cwv consumers.
      ((i:let "wraprec" ((v t) ...) (body ...))
       (begin (set! v t) ... (r5rs-let () body ...)))
      ((i:let "wraprec" vxs body (x formals) cwv ...)
       (call-with-values
           (lambda () x)
         (lambda formals (i:let "wraprec" vxs body cwv ...)))))))

(define-syntax i:named-let
  (let-syntax ((r5rs-let let))
    (syntax-rules (values)
      ;; (i:named-let tag (vx ...) body (bs ...))
      ;;   processes the binding specs bs ... by extracting the variable
      ;;   and expression, adding them to vx and turning the result into
      ;;   an ordinary named let.
      ((i:named-let tag vxs body ())
       (r5rs-let tag vxs . body))    
      ((i:named-let tag (vx ...) body (((values v) x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...)))
      ((i:named-let tag (vx ...) body ((v x) bs ...))
       (i:named-let tag (vx ... (v x)) body (bs ...))))))

;;! Values to list
(define-syntax values->list
  (syntax-rules ()
    ((_ x)
     (call-with-values (lambda () x) list))))

;;! Values to vector
(define-syntax values->vector
  (syntax-rules ()
    ((_ x)
     (call-with-values (lambda () x) vector))))

;; Extra SRFI-71-related macros

;;! All values pairs must satisfy the given 2-predicate
(define-syntax pred2?+
  (syntax-rules ()
    ((_ ?pred ?a ?b)
     (let ((la (values->list ?a))
           (lb (values->list ?b)))
       (let recur ((la la)
                   (lb lb))
         (cond
          ((null? la) (if (null? lb) #t #f))
          ((null? lb) (if (null? la) #t #f))
          (else
           (and (?pred (car la) (car lb))
                (recur (cdr la)
                       (cdr lb))))))))))

;;! All values pairs must satisfy eq?
(define-syntax eq?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eq? ?a ?b))))

;;! All values pairs must satisfy eqv?
(define-syntax eqv?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ eqv? ?a ?b))))

;;! All values pairs must satisfy equal?
(define-syntax equal?+
  (syntax-rules ()
    ((_ ?a ?b)
     (pred2?+ equal? ?a ?b))))

;;! Number of values produced
(define-syntax values-length
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (length v))))))

;;! Extract only the nth-value from a function returning multiple values
(define-syntax values-ref
  (syntax-rules ()
    ((_ n producer)
     (call-with-values
         (lambda () producer)
       (lambda v (list-ref v n))))))
