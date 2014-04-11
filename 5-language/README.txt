Install the "server" directory here as a package. Then, "page.rkt"
should work.

Exercises:

 1. Make the `server` language case-insensitive.

    To make the language case-insensitive, adjust the `reader`
    submodule in "server/main.rkt". Add a wrapper declaration that
    sets the `read-case-sensitive` parameter, which affects the way
    that the default reader parses symols:

      #:wrapper1 (lambda (t)
                   (parameterize ([read-case-sensitive #f])
                     (t)))
