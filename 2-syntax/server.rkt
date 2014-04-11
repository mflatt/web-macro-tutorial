#lang racket
(require web-server/servlet-env
         web-server/http/xexpr
         (for-syntax syntax/parse))

(define-syntax (div stx)
  (syntax-parse stx
    [(div ([attrib attrib-expr]) content-expr)
     (unless (equal? (syntax-e #'attrib) 'style)
       (raise-syntax-error #f "bad attribute" stx #'attrib))
     #'`(div ([attrib ,attrib-expr]) ,content-expr)]))

(serve/servlet
 (lambda (req)
   (response/xexpr
    (div ([style "font-size: xx-large"])
         "Hello"))))

;; Exercises:
;;
;; 1. Define a `smiley` macro that expands to just the number
;;    9786.
;;
;;    For example,
;;      (+ smiley 1)
;;    should produce 9787, but more usefully,
;;      (div ([style "color: blue"]) smiley)
;;    should produce a blue-smiley HTML page.
;;
;;    As a first cut, your `smiley` macro can ignore its syntax-object
;;    argument. Then, improve it so that it complains if `smiley`
;;    is used after an open parenthesis.
;;
;; 2. Define an `rarr` macro that expands to the 'rarr symbol.
;;
;;    Then,
;;      (div ([style "color: blue"]) rarr)
;;    should produce a blue-arrow HTML page.
;;
;; 3. Add an `a` syntactic form that requires an `href` attribute
;;    and produces an `a` X-expression. Use it to hyperlink
;;    "Hello".
;;
;; 4. Make the `style` part of `div` optional.
;;
;;    To do that, use the fact that `syntax-parse` allows a sequence
;;    of pattern clauses, where it uses the first clause that
;;    matches. That is, your revised `div` will have the form
;;
;;      (define-syntax (div stx)
;;        (syntax-parse stx
;;          [(div content-expr)
;;           ......]
;;          [(div ([attrib attrib-expr]) content-expr)
;;           ......])))
;;
;; 5. Allow multiple expressions in the body of `div` or `a`.
;;
;;    To do that, use the fact that `...` in a pattern enables
;;    repetition, where pattern variables from a repetition must
;;    be similarly followed by `...` in a template.
;;
;;    For example, here a `b` macro that doesn't allow any
;;    attributes, but does allow multiple content expressions:
;;
;;       (define-syntax (b stx)
;;         (syntax-parse stx
;;           [(b content-expr ...)
;;            #`(b ,content-expr ...)]))
;;
;; 6. Change `a` to allow any number of attributes, as long as
;;    each attribute is either `href` or `style`.
;;
;;    The `member` function may be useful; it checks whether a
;;    given value is in a given list.