
        Currently Known Errors and Omissions in UMASS Scheme.
===============================================================================

         CONTENTS - (Use <ENTER> g to access required sections)

 --  The GOOD news
 --  However the BAD news follows:
 --  (1) The number system in scheme is incomplete.
 --  (2) list->string and string->list should work with characters.
 --  (3) Error reporting is not perfect...
 --  (4) Vectors are not implemented.
 --  (5)   An isolated close bracket is treated as an identifier.
 --  (6) Input and Output
 --  (7) Missing special forms
 --  (8) Tracing polyadic functions
 --  (9) The cond construct does not allow =>
 --  (10) for-each should be polyadic.
 --  (11) ved_f does not find Scheme functions.
 --  (12) Certain of the cad..r combinations are missing

The GOOD news
-------------
UMASS Scheme does work to to the IEEE Standard in all the functions
tested in the file "examples.scm".

However the BAD news follows:
-----------------------------

(1) The number system in scheme is incomplete.
----------------------------------------------
(1.0) At present UMASS Scheme inherits the Common Lisp number system which is
standard in Poplog. Scheme has a similar number system but makes some changes,
based on a classification of numbers into -exact- and -inexact-. Almost all
operations that involve an inexact number return an inexact result. The
difference can be seen here:

      (max 3.5 4)  ===> 4              COMMON LISP
      (max 3.5 4)  ===> 4.0            Scheme.

(1.1) The lexical analyser for Scheme numbers is incomplete.

(1.2) Many arithmetic functions are not implemented.

(2) list->string and string->list should work with characters.
--------------------------------------------------------------
The non-standard built-in functions named above convert a string to the list
of integer ascii-codes which correspond to the string. This is allowable, but
it would be more consistent with the IEEE standard if character-lists were
produced instead.


(3) Error reporting is not perfect...
-------------------------------------
We still print out Calling sequence:  where no callers will be printed.

In the message:

    Error: function " compile_args_to_par  " called with wrong number of
    arguments

We need to specify how many were expected, how many given.

(4) Vectors are not implemented.
----------------------------------

(5)   An isolated close bracket is treated as an identifier.
------------------------------------------------------------
        )
    Uninitialised variable ')

MIT Scheme simply ignores this case. The formal syntax of Scheme requires an
error report.

(6) Input and Output
--------------------

If input from a file alternates between read and read-char, certain characters
can be lost.

Some of the standard functions are not implemented.

(7) Missing special forms
--------------------------
The "do" constuct is missing.

(8) Tracing polyadic functions
-------------------------------

The trace function does not treat Scheme polyadic functions correctly.

(9) The cond construct does not allow =>
------------------------------------------
A clause in cond can have the form (<test> => <expr>) this is not implemented.

(10) for-each should be polyadic.
----------------------------------
(11) ved_f does not find Scheme functions.
-------------------------------------------

(13) Local recursive definitions may not work.
-----------------------------------------------
The "free-walk" function in Abelson and Sussman p373 seems knackered. Use
letrec instead.
