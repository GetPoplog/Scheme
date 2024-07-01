
; Static Errors in order of mishap_scm calls in compiler
; some lexical errors are missing.
(define (fred x)
  #\funny):
Error: Unknown character name #\funny

(define (fred x)
   '(x . y . z))


Error: Improperly formed dotted list . y  .
Calling sequence:
(no Scheme functions in call-chain)
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.



(

Error: End of input encountered while reading sexpr
Culprits:
Calling sequence:
(no Scheme functions in call-chain)
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.



(define (fred x
  (+ x t)))

Error: define statement "(define (fred x (+ x t)))" defines nothing
Calling sequence:
(no Scheme functions in call-chain)
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.


(define (fred x
  (+ x t)) (+ x 23))
Error: trying to declare expression (+ x t) as variable
Calling sequence:
(no Scheme functions in call-chain)
In or around line 41: (lambda (x (+ x t)) (+ x 23))
While defining 'fred '
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.



(define (fred x)
  (if (4 5)  6 7))

Error: cannot apply 4 as function
Calling sequence:
(no Scheme functions in call-chain)
In or around line 53: (4 5)
While defining 'fred '
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.


(define (fred x)
   (cond
     ((< x 0) (+ x 3))
     ((= x 0))
   )
)


Error: wrong form for expression sequence ()
Calling sequence:
(no Scheme functions in call-chain)
Compiling (<test> <value>) clause ending at line 67:
    ((= x 0))
In or around line 68: (cond ((< x 0) (+ x 3)) ((= x 0)))
While defining 'fred '
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.

(define (fred x)
  (car (quote 45 56)))

Error: Wrong form for quoted expression "(quote 45 56)"
Calling sequence:
    edit
In context: (car (quote 45 56))
While defining 'fred '


(define (fred x)
  (car (quasiquote 45 56)))

Error: Wrong form for quoted expression "(quasiquote 45 56)"
Calling sequence:
    edit
In context: (car (quasiquote 45 56))
While defining 'fred '

(define (fred x x)
   (+ x 3)
)

Error: lambda expression has repeated argument "x " in "(x x)"
Calling sequence:
(no Scheme functions in call-chain)
Compiling argument list ending at line 101:
    (x x)
In or around line 103: (lambda (x x) (+ x 3))
While defining 'fred '
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.

(define fred)
??? this is WRONG - but should be OK with next version.
Error: too many forms in define statement

Culprits:
Calling sequence:
    edit
While defining 'fred '


Error: too many forms in define statement:
(define fred (x) (+ x t))

Calling sequence:
(no Scheme functions in call-chain)
While defining 'fred '
This error report was prepared for Robin Popplestone
 by Jeremiah Jolt, your compile-time helper.


(define (if x y z) (iff x y z))
Error: Attempting to redefine reserved word 'if
Calling sequence:
    edit
While defining 'if '

(define 34 (+ 4 5))
Error: Need variable name or expression instead of "34" in definition
     "(define 34 (+ 4 5))"
Calling sequence:
    edit
While defining '34'

(define (fred x)
   (if 1 2 3 4 ))


Error: Badly formed "if" expression (if 1 2 3 4)
Calling sequence:
    edit
In context: (if 1 2 3 4)
While defining 'fred '

(define (mary x)
   (case (+ 3 4) )
)
Error: case expr "(case (+ 3 4))" needs actual cases
Calling sequence:
    edit
In context: (case (+ 3 4))
While defining 'mary '

(define (anne x y)
  (case x
    a)
)

(define (jane x y)
   (cond 23)
)
Error: Wrong form for condition-value pair "23" in (cond ...) expression
     (cond 23)
Calling sequence:
    edit
Compiling: 23
In context: (cond 23)
While defining 'jane '


(define (fred x)
   (else 34))

Error: "else" apparently used as a function in (else 34). Badly formed
     cond or case?
Calling sequence:
    edit
While defining 'fred '

(define (fred x)
   (let ((y 4)))
)
Error: Missing binding or body in let expr (let ((y 4)))
Calling sequence:
    edit
In context: (let ((y 4)))
While defining 'fred '


(define (fred x)
   (let ((y 4) 3)
    7
   )
)

Error: Badly formed binding 3 in bindings ((y 4) 3)
In file: /users/users3/fac/pop/poplocal/local/Scheme/test_errors.scm

Calling sequence:
    edit
In or around line 207: (let ((y 4) 3) 7)
While defining 'fred '

(define (fred x)
   (let ((y 4) (3))
    7
   )
)
??? trap this one
Error: WORD NEEDED
Culprits: 3,
Calling sequence:
    edit
In context: (let ((y 4) (3)) 7)
While defining 'fred '


(define (fred x)
   (let bob (+ x y))
)

Error: Bindings bob  should be a list
In file: /users/users3/fac/pop/poplocal/local/Scheme/test_errors.scm

Calling sequence:
    edit
In or around line 233: (let bob (+ x y))
While defining 'fred '

(define (fred x)
   (set! a b c)

   )
)

Error: Wrong form (  (set! a b c)  ) for "set!" statement
Calling sequence:
    edit
Compiling: (set! a b c)
While defining 'fred '

(define (fred x)
   (set! '(a b) c)
   )
Error: Non-atom '(a b) cannot be bound by "set!" in (set! '(a b) c)
Calling sequence:
    edit
Compiling: (set! '(a b) c)
While defining 'fred '


(define (fred x)
   (set! if c)
   )



(define (example expr val)
    ;; Allows you to run and check examples of code in-line
    (let (
         (val)( (eval expr (nearest-repl/environment)))
         )
        (newline)
        (display "example: ")
        (write expr)
        (display " = ")
        (write val)
        (if (equal?  val-actual val)
            (begin (display ",  ok!") (newline))
            (begin
                (display "
                 example failed, evaluating: ")
                (write expr)
                (display
                    "
                 value returned: ")
                (write val-actual)
                (display
                    "
                 value expected: ")
                (write val)))))


;DYNAMIC CHECKS

(define (fred x)
   (+ x 34)
)
(fred 'a)


(define (funny list)
   (funny (cdr list))
)

(funny '(1 2 3 4))
