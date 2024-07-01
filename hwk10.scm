

                                 Homework 10
                             CMPSCI 287 SPRING 1997
                      Due Thursday 24APR at 11:59 PM.

         Put  your answers in your cs287 directory as hwk10.lscm

-----------------------------------------------------------------------------


Lecture 16 contains the code for an interpreter for a Scheme-like language.
You are to add the let construct to this. Here is an example to try:


(example
    '(eval_eager
        '(let ((x (+ 3 4))  (y (+ 6 7)))  (+ x y))
        )
    20)

Your code ____must work with an arbitrary number of variables in the let-binding.

____Hint  the above computation is exactly equivalent to:

(eval_eager
    '((lambda (x y) (+ x y)) (+ 3 4) (+ 6 7)))


____Note Recall that the ".lscm" file extension stands  for ________literate ______Scheme.
That is to say, the compiler only interprets as Scheme expressions anything
beginning with a left parenthesis at the start of a line of text. You may
find it convenient to copy section 3 of the Lecture 16 into your hwk10.lscm
file, and then develop your own code in a 'temp.scm' file (or whatever you
want to call it). Some aspects of the development environment are different
for '.lscm' files, and this will avoid confusion. At the end you can paste
your code into the hwk20.lscm for submission.
