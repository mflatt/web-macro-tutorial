#lang slideshow
(require slideshow/code
         slideshow/repl)

(define repl (make-repl-group))

(define larger-font-size 32)
(define default-font-size 28)
(define smaller-font-size 24)

(define file-background "lightcyan")

(define (in-file name content)
  (define color file-background)
  (define border-color "black")
  (define name-box
    (if name
        (inset (scale (if (string? name)
                          (tt name)
                          name)
                      0.8)
               6 3)
        (blank)))
  (define background
    (colorize (filled-rectangle (+ (pict-width content) (current-font-size))
                                (+ (pict-height content) (current-font-size)))
              color))
  (define c
    (cc-superimpose
     (frame background)
     content))
  (define r
    (if name
        (let ([n name-box])
          (define f
            (cc-superimpose (frame
                             (colorize (filled-rectangle (pict-width n)
                                                         (pict-height n))
                                       "beige"))
                            n))
          (vr-append (inset f (min 0 (- (pict-width c) (pict-width f))) 0 0 0)
                     c))
        c))
  r)

(define (sole-repl-area #:height [h (* (+ 5 default-font-size) 4)]
                        #:width [w (* client-w 2/3)]
                        #:font-size [font-size default-font-size]
                        . content)
  (apply repl-area
         #:font-size font-size
         #:height h
         #:width w
         #:make-namespace (lambda ()
                            (define ns (make-base-empty-namespace))
                            (parameterize ([current-namespace ns])
                              (namespace-require 'racket))
                            ns)
         content))

(define (normal-result-area #:width [w (* client-w 2/3)])
  (result-area repl
               #:width w
               #:height (* client-h 1/4)))

(define small-w (* client-w 1/3))
(define small-h (* client-h 1/3))

(define med-w (* client-w 1/2))
(define med-h (* client-h 1/2))

(define big-w (* client-w 2/3))
(define big-h (* client-h 1/2))

(define (two-modules a-mod b-mod
                     #:font-size [font-size #f]
                     #:width [width small-w]
                     #:left-width [left-width width]
                     #:right-width [right-width width]
                     #:height [height small-h]
                     #:left-height [left-height height]
                     #:right-height [right-height height])
  (hc-append
   gap-size
   (normal-module-area a-mod 
                       #:font-size font-size
                       #:width left-width 
                       #:height left-height)
   (normal-module-area b-mod
                       #:font-size font-size
                       #:width right-width
                       #:height right-height)))

(define (normal-module-area mod
                            #:font-size [font-size #f]
                            #:width w
                            #:height h)
  (in-file
   (scale (tt (module-backing-module-name mod)) 0.85)
   (module-area mod
                #:width w
                #:height h
                #:font-size (or font-size default-font-size)
                #:background file-background)))

(define-syntax-rule (scode e ...)
  (scale (code e ...) 0.9))

(define syntax-objects-title "Syntax Objects")

(slide
 #:title syntax-objects-title
 (item "Variable reference:")
 (sole-repl-area "x")
 (item "Symbol s-expression:")
 (sole-repl-area "'x")
 (item "Syntax object:")
 (sole-repl-area "#'x"))

(define WIDE (* client-w 0.9))
(define REPL-WIDE (* client-w 0.85))

(slide
 #:title syntax-objects-title
 (item #:width WIDE "Function call:")
 (sole-repl-area #:width REPL-WIDE "(+ 1 2)")
 (item #:width WIDE "List s-expression:")
 (sole-repl-area #:width REPL-WIDE "'(+ 1 2)")
 (item #:width WIDE "Syntax object:")
 (sole-repl-area #:width REPL-WIDE "#'(+ 1 2)"))

(slide
 #:title "Syntax Backquote and Escape"
 (sole-repl-area #:height big-h))
;; #'x
;; (syntax-e #'x)
;; #'2
;; (syntax-e #'2)
;; #'(+ 1 2)
;; (syntax-e #'(+ 1 2))
;; (datum->syntax #'here 1)

;; ----------------------------------------

(define now-mod
  (make-module-backing
   repl
   #:module-name "now.rkt"
   "#lang racket"
   ""
   "(define now"
   "  (current-seconds))"
   ""
   "now"))

(slide
 #:title "Macros"
 (normal-module-area now-mod 
                     #:width med-w
                     #:height med-h)
 (normal-result-area))
;; Try `now (sleep 1) now'
;; Change `now' to a macro
;;  Aside:
;;   `(now 1)` is allowed...
;;   (if (symbol? (syntax-e stx))
;;       #'(current-seconds)
;;       (raise-syntax-error 'now "bad syntax"))
;;  Change it back for simplicity

(define then-mod
  (make-module-backing
   repl
   #:module-name "then.rkt"
   "#lang racket"
   ""
   "(define-syntax (then stx)"
   "  (current-seconds))"
   ""
   "then"))

(slide
 #:title "Compile-Time Expressions"
 (normal-module-area then-mod
                     #:width med-w
                     #:height med-h)
 (normal-result-area))
;; Fix bug by using `datum->syntax` #`#,
;; `then', `(sleep 1)' , `then'
;; Define `around-then' using quasisyntax

(slide
 #:title "Compile-Time Imports"
 (normal-module-area then-mod
                     #:width big-w
                     #:height med-h)
 (normal-result-area))
;; Change `racket' to `racket/base'
;;  ... add `(require (for-syntax racket/base))'
