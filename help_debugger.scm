
                The UMASS Scheme Debugger


         CONTENTS - (Use <ENTER> g to access required sections)

 --  Printing out a trace of function entry and exit.
 --  Executing a program step-by-step
 --  The Debugger Control Panel.
 --  Using the Debugger from a Terminal

Printing out a trace of function entry and exit.
-------------------------------------------------

Consider the function definition:

(define (sum l)
    (if (null? l) 0
        (+ (car l) (sum (cdr l)))
        ))

The trace facility can be invoked by the Scheme special form:

    (trace sum)

or by using the Debugger control panel, as described below. In either case
evaluating:

(sum '(1 2 3))

will result in the following print out:

(sum  (1 2 3) )
|(sum  (2 3) )
| (sum  (3) )
| |(sum  () )
| |sum   = 0
| sum   = 3
|sum   = 5
sum   = 6

That is to say, as the sum function is entered, its arguments are printed.
As it is exited, its result is printed.  You can trace several functions
at once. To take traces off, do <enter> :untraceall in VED. Do not forget
the colon.

You can trace several functions at one time.


Executing a program step-by-step
---------------------------------

If you want to see more detail of your program on a step-by-step basis,
you should compile it in "debugging" mode (this is the default). This mode
generates more machine code for a given function, and it runs more slowly, but
this should not be a problem for beginners at Scheme programming.

When a break-point is encountered, Scheme types out a report on the current
state of your computation, in the output.scm file. An example is below, with
explanatory comments. All line numbers are given with respect to the file
containing the function.


BREAK at line: 2                      ___________computation _______stopped __at ____line _2
expression (null? list)  = <false>    ______having _________evaluated ____this __________expression

Variable Frame:                       _________variables __of ___the _______current ____call __of sum
    list  = (3 4)
END of frame for function sum
_________________________________________________________

Variable Frame:                       _________variables __of ___the _______calling ____call __of ___sum
    list  = (2 3 4)
END of frame for function sum  - (LAST FRAME)  _____limit ____from Number of Frames
_________________________________________________________


The Debugger Control Panel.
-----------------------------
This control panel has the following features:

Trace:  Type the name of a function to be traced here. Hit return and all
entries to and exits from the function will be traced.

Untrace: Stop tracing the function whose name is given in the Trace: slot
above.

Report: This is a write-only field in which status reports are made.

Generate Debug Code: Turns on and off the generation of code to support
debugging. By default, debugging code __is generated. A break-point is generated
in the code for every non-atomic expression encountered.

Stop at All Breakpoints: If this button is set true the program will stop
at every breakpoint. If false it will stop at no breakpoints.

Abort: Hit this button to stop the current computation, and reset Scheme.

Number of Frames: This controls the number of variable frames that will be
printed when you stop at a breakpoint.

Go To Next Breakpoint: Continue executing Scheme until the next breakpoint is
encountered.

Help: Display this file.

Report POP-11 functions: This is primarily for system maintenance.

Using the Debugger from a Terminal
----------------------------------
If the debugger is used from a terminal, rather than running under X-windows,
a simpler form of some of the messages is used.

The user can control what happens at breakpoints by typing one of
the following characters:

         a    = abort current computation
         f    = print current variable frame
         f<n> = print n variable frames
         n    = proceed to next break-point.
         s    = skip - do not break until exit from current function
