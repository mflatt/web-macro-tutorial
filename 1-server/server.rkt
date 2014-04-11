#lang racket
(require web-server/servlet-env ; to get `serve/servlet`
         web-server/http/xexpr) ; to get `response/xexpr`

(serve/servlet
 (lambda (req)
   (response/xexpr
    '(div ([style "font-size: xx-large"])
          "Hello" 9786))))

;; Exercises:
;;
;; 1. Add more words and make some of them bold or italic using the
;;    S-expression form of <b> and <i>.
;;
;; 2. Make "Hello" a hyperlink to some web site using the S-expression
;;    form of <a> with attribute `href`.
;;
;; 3. Include a smiley in the server's output by using using the
;;    number 9786 (or written in hexidecimal as `#x263A`). Integers
;;    used as content are treated as numerical HTML entities.
;;
;; 4. Include an right-arrow in the server's output by using the
;;    symbol 'rarr as content. Symbols that are used as content (i.e.,
;;    not after an open parenthesis) are treated as HTML entity names.
;;
;; 5. The `current-seconds` function returns the current time in
;;    seconds since the epoch, and the `number->string` function
;;    converts a number to a string. Use those function to make the
;;    server response include the time (in seconds since the epoch)
;;    that the request was processed.
