#lang racket
(require web-server/servlet-env
         web-server/http/xexpr
         (for-syntax syntax/parse))

(provide div a
         (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax (define-tag stx)
  (syntax-parse stx
    [(define-tag tag ok-attrib ...)
     #`(define-syntax (tag stx)
         (syntax-parse stx
           [(tag ([attrib a-expr] (... ...)) content-expr)
            (for-each (lambda (attrib-stx)                   
                        (unless (member (syntax-e attrib-stx) '(ok-attrib ...))
                          (raise-syntax-error #f
                                              "bad attribute"
                                              stx
                                              attrib-stx)))
                      (syntax-e #'(attrib (... ...))))
            #'`(tag ([attrib ,a-expr] (... ...))
                    ,content-expr)]))]))

(define-tag div style)
(define-tag a href style)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(module-begin expr)
     #'(#%module-begin
        (start (lambda () expr)))]))

(define (start reply-thunk)
  (serve/servlet
   (lambda (req)
     (response/xexpr
      (reply-thunk)))))

;; Exercises:
;;
;; 1. Without changing "page.rkt", cause the server to print "request"
;;    to stdout each time the server receives a page request. The
;;    `displayln` function takes a string and prints the string plus a
;;    newline.
;;
;; 2. Disallow literals other than strings in "page.rkt" by replacing
;;    `#%datum`, which is implicitly wrapped around a literal in a
;;    program.
;;
;;    Your replacement `datum` will be something like
;;       (define-syntax (datum stx)
;;         (syntax-parse stx
;;          [(datum . literal) ....]))
;;    Note the `.` after `datum`, which is needed due to the way that
;;    `#%datum` is wrapped around a literal. Expand a string literal
;;    to use `#%datum` followed by a dot, and anything else should be
;;    a syntax error.
;;
;;    For example,
;;       (div () 42)
;;    should be a syntax error that highlights the `42`.
