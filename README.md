Spend a few minutes preparing for the tutorial...

Installation
------------

Install Racket v6.0 or later from 

 * [http://download.racket-lang.org/](http://download.racket-lang.org/)


Preparing DrRacket (1 minute)
-----------------------------

After installing, start DrRacket, and make sure that it's in **The
Racket Language** mode. To select that mode, especially if it's your
first time starting DrRacket, select **Choose Language...** from the
**Language** menu, and then pick the option at the top of the dialog.
Then, the top half of DrRacket's window should have

    #lang racket

Click the **Run** button, and you should see something like the
following in the bottom half of DrRacket's window:

    Welcome to DrRacket
    Language: racket
    > 


Using DrRacket (2 minutes)
--------------------------

The top area in DrRacket's window is called the _definitions area_.
Add the number `42` to the to the definitions area, so that it reads
as follows:

    #lang racket
    42

and click the **Run** button (or hit F5 or Ctrl-R or Cmd-R). The
bottom area in DrRackete's window is called the _interactions area_ or
_read-eval-print loop_ (_REPL_), and it will look like this:

    Welcome to DrRacket
    Language: racket
    42
    > 

That's because `42` is Racket expression, evaluating the expression
produces the value 42, and the value is shown in the interactions
window. You could also type the expression `43` after the `>` prompt
in the interactions area, and the value of that expression will be
shown:

    Welcome to DrRacket
    Language: racket
    42
    > 43
    43
    >

As `42` and `43` illustrate, the definitions and interactions areas
support mostly the same kinds of expressions. There are some technical
differences, because DrRacket can see everything in the definitions
area at once, while it sees input in the interactions window
incrementally.

More significantly, when you click **Run**, the interactions area is
reset, while the definitions area is the part you can save as a source
file. So, we put our program in the definitions area, and we use the
interactions area to explore it.


If You've Seen Lisp/Scheme/Racket Before
----------------------------------------

You're ready to go. The rest of this is a crash course in Racket
notation for those who have never seen parentheses before.


Function Calls and Definitions (5 minutes)
------------------------------------------

Racket is a Lisp, so expressions and other syntactic forms are
generally formed by using parentheses and putting a form name or
function/operator to call after the open parenthesis.

For example, you can get 43 by taking the absolute value of -43
using the `abs` functions:

    Welcome to DrRacket
    Language: racket
    > (abs -43)
    43
    >

Or you could get the same result by adding 42 and 1 with with `+`
function:

    Welcome to DrRacket
    Language: racket
    > (+ 42 1)
    43
    >

It's important to put a space after `+`, because `+43` means positive
43. Also, extra parentheses are never ok, because parentheses mean
function calls. That is, `(43)` would mean calling the function 43
with zero arguments (but 43 is not a function, so it's an error).

To define your own function, typically in the definitions area, use
the `define` form, and then your function can be called just like the
predefined functions:

    #lang racket

    (define (negabs x)
      (- 0 (abs x)))

    (negabs 42)

A more Rackety name for a negative-absolute function would use a
hyphen:

    #lang racket

    (define (neg-abs x)
      (- 0 (abs x)))

    (neg-abs 42)

The `-` in `neg-abs` is treated as part of the name, while a `-` to
mean the subtraction function must be separated by spaces or
parentheses. Note that the `-` function is overloaded to work on zero
arguments, so the above program could also be

    #lang racket

    (define (neg-abs x)
      (- (abs x)))

    (neg-abs 42)

When you click **Run** for that program, the result of `(neg-abs 42)`
prints, and `neg-abs` is available for further exploration:


    Welcome to DrRacket
    Language: racket
    -42
    > (neg-abs 99)
    -99
    >


Other Values (1 minute)
-----------------------

Strings are written with double quotes: `"hello"`

Booleans are written as `#t` or `#f`.

Symbols are similar to strings, but written with a leading single
quote, and with the same delimiting rules as identifier: `'hello`.
    
    Welcome to DrRacket
    Language: racket
    > "hello"
    "hello"
    > (string-append "a" "b")
    "ab"
    > (> 2 1)
    #t
    > (< 2 1)
    #f
    > (= 2 2)
    #t
    > 'hello
    'hello
    > 'abs
    'abs
    > '+
    '+


Lists (5 minutes, optional)
---------------------------

_You can stop here. We'll cover this part at the start of the the tutorial._

The name `empty` is defined as the empty list, and the `cons` function
adds an item to the from of a list. The `first` function extracts the
first item of a (non-empty) list, and the `rest` function extracts all
items of a (non-empty) list except the first.

    > (first (cons "one" empty))
    "one"
    > (first (rest (cons "one" (cons "two" empty))))
    "two"

The `list` function is a shorthand; it takes any number of arguments
and puts them in a list, so you don't have to `cons` each item
individually.

    > (first (rest (list "one" "two" "three")))
    "two"

Lists usually print in a more compact form than using `empty` and
`cons` or even `list`. They print using a single quote --- the same
one used for symbols --- followed by an open parenthesis, each list
item, and a closing parenthesis.
    
    > (cons "one" empty)
    '("one")
    > (cons 1 empty)
    '(1)
    > (rest (list "one" "two" "three"))
    '("two" "three")

The single quote used to start a list is implicitly added to any
symbol or list inside of a printed list, which makes the printing of
such lists especially compact:

    > (list 'one 'two 'three)
    '(one two three)
    > (list 'one (list 2 'three) "four")
    '(one (2 three) "four")

You can use a single quote instead of `cons` and `list` to form a list
expression. Note that the elements of such a list are not themselves
expressions, though, since the quote distibutes to the
elements. Instead, identifiers and parentheses in the quoted list form
symbols and lists:

    > '(1 2 3)
    '(1 2 3)
    > '(one two three)
    '(one two three)
    > '(1 (abs -2) 3)
    '(1 (abs -2) 3)
    > (first (rest '(1 (abs -2) 3)))
    '(abs -2)
    > (first (first (rest '(1 (abs -2) 3))))
    'abs

If you want both a shorthand for a list and an expression to be
evaluated within the list, use a backquote instead of a quote, and
then use a comma to escape to expression mode:

    > `(1 ,(abs -2) 3)
    '(1 2 3)
    > `(1 (2 (3 ,(+ 2 2)) 5) ,(abs -6))
    '(1 (2 (3 4) 5) 6)
