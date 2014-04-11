#lang racket
(require web-server/servlet-env
         web-server/http/xexpr
         (for-syntax syntax/parse))

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

(serve/servlet
 (lambda (req)
   (response/xexpr
    (div ([style "font-size: xx-large"])
         (a ([href "http://racket-lang.org"]
             [style "color: red"])
            "Hello")))))

;; Exercises:
;;
;; 1. Define `html` and `body` for forming proper HTML.
;;
;;    These forms don't support any attributes (unless you really want
;;    to support `manifest` and `xmlns` in `html`), but an empty
;;    attribute sequence will be required to use them:
;;       (html () (body () "Hello"))
;;
;; 2. Adjust `define-tag` so that multiple content expressions are
;;    allowed.
;;
;; 3. Adjust `define-tag` to that it allows empty attribute
;;    parentheses to be omitted.
;;
;;    That is,
;;       (html (body (div ([style "color: blue"])
;;                     "Hello")))
;;    should work, or even
;;       (html (body (div "Hello")))
;;
;;  4. Improve the static checking in `div`, `a`, etc., to check
;;     that no attribute is duplicated.
;;
;;     For example,
;;        (div ([style "color: red"]
;;              [style "color: blue"])
;;          "Hello")
;;     should be a syntax error that higlights the second `style`.
;;
;;     The function `check-duplicate-identifier` will be useful. It takes
;;     a list of identifiers (i.e., symbols still in syntax-object form)
;;     and returns either #f or an indentifier that is a duplicate in the
;;     list.
