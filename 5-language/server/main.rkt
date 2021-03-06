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

(module reader syntax/module-reader
  server)
