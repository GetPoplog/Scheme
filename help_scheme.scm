
         CONTENTS - (Use <ENTER> g to access required sections)

 --  How to start up UMASS Scheme.
 --  Editing Files and Running Scheme Code.
 --  Changing your default font for VED under X.
 --  Simple values in Scheme
 --  Variables in Scheme
 --  Variable Bindings
 --  Expressions in Scheme
 --  So what are functions in Scheme?
 --  Making your very own function


How to start up UMASS Scheme.
------------------------------

To start up UMASS scheme type scheme as a Unix command. You will get a message
like this:

  Sussex Poplog (Version 14.5 Tue Feb 22 15:01:28 WET 1994)
  UMASS Scheme (Version 1.0)
  initialising X windows

  Sussex XVed (Version 2.0 (motif), SCHEME Tue Sep 12 12:14:16 EDT 1995)
  X Server: MIT X Consortium
  initialising X windows


[Note for  experts: getting  UMASS Scheme  depends on  an alias  in the  class
.cshrc file, which  itself depends on  your own ~/.cshrc  file. If you  change
this file, you may find yourself running  a version MIT Scheme which does  NOT
accord with the IEEE standard required for this class.

UMASS Scheme is implemented under the Sussex Poplog system developed at Sussex
University (Falmer  UK), and  employs  the X-window  system developed  at  MIT
(Cambridge MA). ]

If you are logged in in the EDLAB, you will get a version which is  customised
for running under X-windows.  In this version,  you will be  able to edit  and
test programs entirely within UMASS Scheme system if you wish.

If you are  logged in  remotely, for  example from home,  and do  not have  an
X-server on your local machine, you will  get a version of UMASS Scheme  which
is customised for running under a terminal emulator. If your emulator  matches
a VT100  terminal, this  should be  OK, but  you may  need to  set the  "term"
environment variable before you run your Scheme. If Scheme is unhappy
with your terminal it will say so.

   setenv term vt100

If you are logged in remotely from a machine which is running an X-server, for
example another workstation) then you should do:

   setenv DISPLAY mymachine

where mymachine is the net-address of the machine you are logged in on. This
will allow UMASS Scheme to open windows on your machine.

Editing Files and Running Scheme Code.
-----------------------------------------

Under X-windows, a small  "control panel" will pop-up  when you start  Scheme.
This has a  box labelled "File:".  This contains  the name of  a default  file
("unnamed.scm") that you can edit using  the built-in VED editor. If you  want
to change this, put the mouse in the  box, drag the mouse along the length  of
the name - it will now reverse its colours. Then type in the name of the  file
you want to edit, and hit the Do Edit button. After a brief pause a new window
will appear with your file (possibly empty) in it.

If it is a file  to contain text in the  Scheme language, the name should  end
with ".scm".

There is also "Exit" button to kill the Scheme system.

Each edit window contains a "menu-bar" across the top. This is used in a style
akin to  that  of  the  Macintosh. Currently  this  has  the  following  menus
attached:

File    Allows you to choose open a new file for editing, to save it to
        backing store, to save it under another name, to write it under
        another name, to insert the contents of a file, and to
        "quit" that is to stop editing that particular file.

        Note that most of the file options use a "File Tool" which allows you
        to select the file to operate on interactively. This looks a bit
        complicated, but you can always type the name of the file you want
        in the Selection: slot, and hit the "Edit" button.

Edit    This menu allows you to cut, copy and paste pieces of your document.
        Again, the style is Macintosh-like. You can paste into other windows
        on your machine which are not related to UMASS Scheme, although you
        should note that the mouse-button conventions are usually different.
        You use the LEFT mouse button to select an arbitrary piece of text.

        You can also select a "range" of text. To do this, hold down the
        control key (ctrl) and use left mouse button to select a number of
        lines of text. The selected range is indicated by a black line that
        appears down the left of your screen.

        A Search function is also provided.

View    This allows you to go to the start or end of the file, or of a
        procedure.

Compile This menu allows you to compile program text. Scheme uses an
        incremental compiler, so it is possible to compile a line of text,
        a single function definition (use the current procedure menu item)
        or the text of a range or selection. You can also choose to compile
        a file selected with a File Tool.

Lectures This contains a list of

cs287   This menu allows you to access information specific to the class.
        It will be added to as time goes on.

Additional help with editing is found under the "287" menu. This online
material is found in the directory $popscheme.


Changing your default font for VED under X.
-------------------------------------------
If you want to use a different font, add a line of the form XVed*font: <font>
to your .Xdefaults file. You can obtain a (long) listing of available fonts
by doing xlsfonts as a Unix command.  You can also change the font of a
particular window by doing <enter> window font <font>.

XVed*font: 7x12    # The default size - rather small for some people
XVed*font: 9x15    # A bigger size, which you may like better.

Simple values in Scheme
------------------------
Scheme operates on the usual simple data-objects, that is to say data-objects
that can best be thought of as having no internal structure.

integers:  These are of _________arbitrary _________precision that is they are only limited
           in size by the total amount of memory you have!

rationals: Scheme can and will compute with fractions. If you divide one
           integer by another you will get a rational. You should usually
           avoid rational computations, unless you are doing symbolic algebra!

reals:     These are represented by floating point numbers.



23   is an integer
3.4  is a real
3/4  is a rational (WARNING - UMASS Scheme does not currently input rationals
                    but it will generate them and print them out).

booleans:   These are #f (false) and #t (true). [NOTE for experts:
            unlike non IEEE Schemes and most Lisps, the empty list '()
            is not a false truth value.]


Variables in Scheme
-------------------
A scheme variable is  a sequence of  letters (a..z A..Z)  or digits (0..9)  or
signs (+  - *  / ...)  which are  terminated by  "white-space" or  by  certain
special characters, of which parentheses are the most important. In fact, most
combinations of non-whitespace characters which  Scheme does not recognise  as
being a number are treated as a variable. Usually, people will choose  helpful
names for variables.

For example the following are variables:

        x y x_23 34x the-fat-cat + +1

Warning - Scheme gives some variables _______initial ______values, for example +,*,/ . It
is not a good idea to use these variables for your own purposes!


Variable Bindings
-------------------
Variables usually have values. We  say that a variable  is "bound" to a  value
when it  is associated  with the  value. Scheme  has a  STATEMENT for  binding
variables

    (define <variable> <expression>)

For example:

    (define pi 3.14159)

binds the variable pi to have the value 3.14159

Expressions in Scheme
----------------------
Expressions have  a very  uniform syntax  in Scheme.  An expression  can  be a
constant or a variable,  or it can  be a sequence  of expressions enclosed  in
parentheses (sometimes called a ___________combination).

In the case of a combination, the  first expression is regarded as a  ________function
while the others are its _________arguments. Thus:

     (+ 3 4)

is an expression, whose function is '+' and whose arguments are 3 and 4.

When Scheme evaluates an  expression of this form,  it evaluates the  function
and arguments, and then _______applies the function to the arguments. Evaluating  the
above expression we get

     7

Likewise:

    (- 5 4)

means "the result of subtracting 4 from 5", or in ordinary mathematical
notation 5-4.

A more complicated example is:

(+ 3 (* 4 5))

Here we evaluate (* 4 5) to get 20, and then add 3 to get 23.

So what are functions in Scheme?
---------------------------------
Well, actually functions are ______pieces __of _______machine ____code (in effect subroutines for
those who have taken CMPSCI 201). So the value of the variable '+' is a  piece
of code for doing addition.

[Note you  can  actually  redefine  Scheme standard  functions.  This  is  NOT
recommended for novices, for you may lose important capabilities]

Making your very own function
-----------------------------
A Scheme function is defined by the syntax

    (lambda <formals> <body>)

here <formals> is (usually) a  sequence of variables enclosed in  parentheses,
and <body> is  a sequence of  expressions. For the  __________functional use of  Scheme,
only one expression is needed in the body.

Note that this is ___not an application of the function 'lambda', but is what  is
called a "special form" in Scheme.

For example

    (lambda (x) (* x x))

is the function which squares its argument. So


    ( (lambda (x) (* x x))  3)

evaluates to  9. The  rule  for evaluation  of  a lambda-function  applied  to
arguments is to (a) evaluate all the arguments, and (b) strip off the 'lambda'
and substitute the values of each argument for the corresponding formals,  (c)
evaluate the body of the lambda with the values substituted.

   ( (lambda (x) (* x x)) (+ 3 4))

Evaluate the argument

   ( (lambda (x) (* x x)) 7)

Strip and substitute for corresponding formals (x=7) :

    (* 7 7)

Evaluate body

    49

This may seem a funny way of defining functions - in Pascal we would give  the
corresponding function a name, like "square". In Scheme, this is easily done

(define square (lambda(x) (* x x))  )

Scheme responds:

<Compiled function: square >

and now we can use it

    (square 4)

Scheme responds:

    16

An example of a lambda expression with more than one argument is

    (lambda (x y) (+ x (* 2 y))

And an example of its use is:

    ((lambda (x y) (+ x (* 2 y))) (+ 4 3) 5)

Evaluate the arguments

    ((lambda (x y) (+ x (* 2 y))) 7 5)

Strip and substitute, x=7, y=5:

     (+ 7 (* 2 5)))

and evaluate

     (+ 7 10)

     17

The Debugger
------------
This is accessed by hitting the "Debugger" button on the Scheme Control Panel.

Generate Debug Code: This is normally true, so that Scheme generates code for
the debugger by default. Turn it off to create smaller, faster programs.

Stop at All Breakpoints: This is normally false. When set true, Scheme will
stop after it evaluates every compound expression such as (+ x 2). It will
print out "frames" for the top n  functions you are currently executing, where
n is set by the "Number of Frames" slider.

Number of Frames: This slider determines how many function-frames you will
see.

Go To Next Breakpoint:

Skip Current Function:

Help: Prints out help information about the debugger.
