

                      SuperScheme Version 3.1, 9OCT98


         CONTENTS - (Use <ENTER> g to access required sections)

  1   Release Notes Version 3.1

  2   Naomi Nag provides a critique of your programs
      2.1   Checking the number of arguments
      2.2   If expression with only 2 arguments
      2.3   Expression Sequences need begin

  3   The Integral HTML Browser

-----------------------------------------------------------------------
1  Release Notes Version 3.1,3.2
-----------------------------------------------------------------------

Version 3.1 contains additional code for Naomi Nag's program critique,
intended to encourage you to make use of "begin" where you need it.

Also the system "make" script has been modified to ensure that every
new system is checked automatically with $popscheme/examples.scm,
rather than manually.

Version 3.2 has a pre-compiled version of caddr (also called "third"). This
is a palliative for the intermittent bug reported in class on 4NOV98

-----------------------------------------------------------------------
2  Naomi Nag provides a critique of your programs
-----------------------------------------------------------------------

The version of Scheme available in the UMASS EDLAB provides extra help  for
novice users. Various Scheme  constructs which are  legal Scheme but  which
are probably wrong are  flagged by a component  of the compiler, one  Naomi
Nag. Some of these errors would be later picked up by the run-time system.

2.1  Checking the number of arguments
-------------------------------------

For example:

    (define (wrongsum l)
        (if (null? l) 0 (+ (car(l)) (wrongsum (cdr l )))))

Will produce:

    WARNING (from Naomi Nag)

    The expression

        (l)

    calls the non-defined function   l    with no arguments.
    While this is legal Scheme, you probably didn't mean to do this.


    Your current user level is 0, to prevent repetition of this message do:
       (define level_of_user 2)
    <Compiled function: wrongsum >


    (define (funnysum l)
        (if (null? l) 0 (+ (car l 3) (wrongsum (cdr l )))))

produces

    WARNING (from Naomi Nag)

    The expression

        (car l 3)

    calls the  function   car    with 2 arguments -
    it takes 1 arguments

    Your current user level is 0, to prevent repetition of this message do:
       (define level_of_user 4)
    <Compiled function: funnysum >

2.2  If expression with only 2 arguments
----------------------------------------

This is incorrect, except in the imperative paradigm. The definition:


    (define (wrong_if x)
        (if (= x 0) 1))

will produce:

    WARNING (from Naomi Nag)

    The if expression

        (if (= x 0) 1)

    has only 2 arguments.
    While this is legal Scheme, at your level of use it
    is probably a mistake. It only makes sense in the Imperative Paradigm

    Your current user level is 0, to prevent repetition of this message do:
       (define level_of_user 2)
    <Compiled function: wrong_if >

2.3  Expression Sequences need begin
------------------------------------
Scheme, in sundry places, allows you to write a sequence of expressions
where, in the pure functional paradigm, you might write one. For example

(define (fred l1 l2)
    (car l1) (car l2))

is legal Scheme, but the first call of "car" is meaningless. This
definition produces:

WARNING (from Naomi Nag)

The lambda expression

    (lambda (l1 l2) (car l1) (car l2))

has more than one expression in its body

     ((car l1) (car l2))

While this is legal in Scheme, at your level of use of the language
it is probably a mistake.
This construct ONLY makes sense in the Imperative Paradigm,
so that if you really want use it, your program would be clearer if
you used the begin construct

Your current user level is 0, to prevent repetition of this message do:
   (define level_of_user 2)
<Compiled function: fred >

Likewise:

(define (fred l1 l2)
    (cond
        ((null? l1) (car l1) (car l2))
        (else 34)))


WARNING (from Naomi Nag)

You have a sequence of expressions
  ((car l1) (car l2))
 in cond.
This may be a mistake, but if it is not, it's better to enclosethem in
     a "begin" construct

Your current user level is 0, to prevent repetition of this message do:
   (define level_of_user 3)
<Compiled function: fred >


-----------------------------------------------------------------------
3  The Integral HTML Browser
-----------------------------------------------------------------------


An HTML browser is installed in the UMASS SuperScheme system to permit  the
revised lecture notes to be viewed from within the Scheme sytem. This  is a
text-only browser, and is restricted to material  in the Edlab - it is  not
webworthy.
