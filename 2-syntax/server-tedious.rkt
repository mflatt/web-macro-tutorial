#lang racket/base
(require web-server/servlet-env
         web-server/http/xexpr
         (for-syntax racket/base
                     racket/list))

(define-syntax div
  (lambda (stx)
    (define (bad-syntax)
      (raise-syntax-error #f "bad syntax" stx))
    (define l (syntax-e stx))
    (unless (list? l) (bad-syntax))
    (unless (= (length l) 3) (bad-syntax))
    (define as (syntax-e (second l)))
    (unless (list? as) (bad-syntax))
    (unless (= (length as) 1) (bad-syntax))
    (define al (syntax-e (first as)))
    (unless (list? al) (bad-syntax))
    (unless (= (length al) 2) (bad-syntax))
    (unless (equal? 'style (syntax-e (car al)))
      (bad-syntax))
    #``(#,(first l) ([#,(first al) ,#,(second al)])
                    #,(third l))))

(serve/servlet
 (lambda (req)
   (response/xexpr
    (div ([style "font-size: xx-large"]) 
         "Hello"))))
