

                            CMPSCI 287 - SPRING 1995
                           PROGRAMMING ASSIGNMENT 2

                       Due date Thursday 14 March at 12:59 pm.


         CONTENTS - (Use <ENTER> g to access required sections)

 --  Getting "Illegal" tokens
 --  Primitives are identifiers and constants.
 --  What You Have to Do
 --  Some parser-building utilities.
 --  Expressions

This assignment  calls  for  you  to  write  a  parser  for  a  simple
programming language, modelled loosely  on C. It looks  like a lot  of
work, but there is  ONE hard function  of less than  30 lines of  code
that you have to write, and one  tricky function of around 6 lines  of
code. Other code can be found in the class notes. The rest of the work
consists of definitions of a few lines each.

This parser is to be built up out of several sub-parsers as described below.

Getting "Illegal" tokens
---------------------------
Parsers operate on lists of tokens. However, important tokens for most
programming languages include the parentheses  "(" and ")", the  comma
"," and the semi-colon and sometimes "{" and "}", "[" and "]". All  of
these have, or  may have have  a special role  in Scheme.  Therefore a
Scheme function has been provided to give you an easy way of  creating
lists of tokens:

    string->token-list-pop11

It is convenient to bind this function to one with a shorter name:

    (define toks string->token-list-pop11)

If we now evaluate:

    (toks  "a(3,4)")

we obtain:

    (a ##( 3 ##, 4 ##))

Here the "illicit" tokens "("  etc. are flagged by "##"  . Note that '##('  is
not standard Scheme syntax, and cannot be read back in.

It is also useful to have a function that will generate a single such token:

    (define tok (lambda (string) (car(toks string))))

so that

        (tok "{")

will evaluate to the corresponding Scheme symbol.


Primitives are identifiers and constants.
------------------------------------------
You can use the function mk_parser_singleton, defined in lecture6.scm. This
function will be covered on Tuesday  5 March (which still gives you  over a
week to complete  the parser)  to give  you a model  for how  to write  the
functions below. Note that its definition  has been changed in the  on-line
version of the notes to allow for the list-of-tokens being empty. So  get a
fresh definition from the on-line notes!

Note also that  these new definitions  use member? rather  than the  Scheme
standard function member because it is easier to understand the information
the debugger prints out since member? always returns a boolean value.

What You Have to Do
---------------------

(1) Define a  parser identifier  to recognise  an identifier  which is  any
Scheme symbol which is not a reserved word of the language. Reserved  words
are those  mentioned specifically  in the  "productions" which  define  the
language. You will have to read  through the grammar rules below to  make a
list of these reserved words.

    (example '(identifier '()) #f)
    (example '(identifier '(34)) #f)
    (example '(identifier '(x)) '(x))
    (example '(identifier '(+)) #f)

(2) Define a parser constant  which recognises any Scheme atom which is not a
symbol.

    (example '(constant '(x + 7)) #f)
    (example '(constant '(34 + x)) '(34 + x))

Some parser-building utilities.
-------------------------------
You will need to define one or two utility functions for building  parsers,
as well as using mk_parser_singleton and mk_parser_seq discussed lecture 6.


The function mk_parser_seq  will be a model for how you should write these
utilities.

(3) Define the function mk_parser_or which creates a parser. In fact:

    (mk_parser_or parser1 parser2) ==> parser

will build a parser which recognises those languages recognised by parser1  or
by parser2, or more precisely, which tries parser1 on a given list of  tokens,
and if it fails, it then tries parser2. The parse produced remains unchanged.


(4) Define  a parser  primitive  which recognises  either an  identifier
or a constant.


(example '(primitive '()) #f)
(example '(primitive '(34 + x)) '(34 + x))
(example '(primitive '(x)) '(x))
(example '(primitive '(+)) #f)

(Optional - no extra credit but it  may make life easier) Define the  function
mk_parser_sandwich which creates a parser:

    (mk_parser_sandwich symbol_1 parser symbol_2 build_parse) ==> parser

will build a parser to recognise any  list of tokens which consists of a  list
recognised by parser bracketed by symbol_1 and by symbol_2.

The function build_parse will create a new structure from the car of the old.

For example

   (define body (mk_parser_sandwich (tok "{") stmt_list (tok "}")
                   (lambda (x) x)))

will recognise

    { <stmt_list> }

returning whatever <stmt_list> returned.


(5) Define the function mk_parser_alternate

    (mk_parser_alternate parser1 parser2 builder_one builder_ext)

makes a parser for the syntactic form <r> whose grammar is

   <r> -> <r1>
   <r> -> <r1> <r2> <r>

where <r1> is recognised by parser1 and <r2> is recognised by parser2. This is
useful in recognising  common constructs,  for example a  list of  identifiers
separated by commas - x,y,z.

[Hint: this is the hardest piece of code to write. You will need letrec .  You
could try  modifying  mk_parser_seq  or  you  could  try  writing  a  specific
alternate parser, that is where the parser1, parser2 and builder_... functions
are global  parsers and  builders,  debugging that  and then  generalising  by
lambda-abstraction.]


Expressions
-----------
Reserved words:  '+', '-', '*', '/', '(', ')', ',', '{', '}', '?', ':',
                 'int', 'float'

You will need mk_parser_alternate to make these parsers.

(5) Define a parser application according to the following rules:

    <application> -> <identifier> ( <arglist> )

    <arglist> -> <primitive> , <arglist> | <primitive>

    (example '(arglist (toks "x,3")) '((x 3)))
    (example '(arglist '(x y)) '((x) y))

Hint - it helps to define arglist_in_brackets as ( <arglist> )

    (example '(arglist_in_brackets (toks "(x,2)+3")) '((x 2) + 3))
    (example '(arglist_in_brackets (toks "x+3")) #f)

Parsing an application should give you a Scheme representation of the
application. Thus:

(example '(application (toks "f(x,1) + y"))  '((f x 1) + y))
(example '(application (toks "2 + y"))  #f)

(6.1) Define a parser term

    <term> -> <application> | <primitive>

(example '(term (toks "f(x,2)+3")) '((f x 2) + 3))
(example '(term (toks "x+3")) '(x + 3))
(example '(term (toks "+ 3")) #f)


(6.2) Define a parser times_expr

    <times_expr> -> <term> * <times_expr>
                 |  <term> / <times_expr>
                 | <term>


    (example '(times_expr (toks "a*2 end")) '((* a 2) end))
    (example '(times_expr (toks "a*f(3,4)*2 end"))
               '((* a (* (f 3 4) 2)) end))
    (example '(times_expr (toks "*2 end")) #f)

(6.3) Define a parser sum_expr

    <sum_expr> -> <times_expr> + <sum_expr>
               |  <times_expr> - <sum_expr>
               |  <times_expr>


    (example '(sum_expr (toks "a*2 end")) '((* a 2) end))
    (example '(sum_expr (toks "a*2+c end")) '((+ (* a 2) c) end))
    (example '(sum_expr (toks "a + f(3,4)*2 end"))
        '((+ a (* (f 3 4) 2)) end))
    (example '(sum_expr (toks "a-b-c end")) '((- (- a b) c)end)) ; why?
    (example '(sum_expr (toks "*2 end")) #f)

[Hint there is a little catch in this one - getting the answer labelled
"why?" is going to be a problem. You can solve it by making a straight
list of the alternating times_expr  and addition/subtraction operators,
and than writing a function to put this list in the final form. This
is a little tricky.
]

(6.4) Why do we require the particular answer labelled "why?"



(6) Define a parser if_expr

    <if_expr> ->  <sum_expr> ? <sum_expr> : <sum_expr>
              |   <sum_expr>

    (example '(if_expr (toks "b ? x : y ; return 1;"))
                        (cons '(if b x y) (toks "; return 1;")))

    (example '(if_expr (toks "+ 2; y end")) #f )

    (example '(if_expr (toks "x + 2; y nothing"))
         (cons '(+ x 2) (toks "#; y nothing") )) )

    (example '(if_expr (toks "}")) #f)

(7.1) Define a parser expr_stmt

    <expr_stmt> -> <if_expr> ;

The parse-tree returned should be a list of the parse-trees returned by
the component <if_expr>.

    (example '(expr_stmt (toks "x;  return  " )) '(x return))
    (example '(expr_stmt (toks "a;b;c }"))  (cons 'a  (toks "b;c;}")))
    (example '(expr_stmt (toks "b ? x : y; z "))
           ' ((if b x y) z) )
    (example '(expr_stmt '( + end)) #f)


(7.2)   Define a parser stmt_seq

    <stmt_seq> -> <expr_stmt> <stmt_seq>
               |  <expr_stmt>

Hint -  you could use mk_parser_alternate with a parser for the null
language as the alternate.

    (example '(stmt_seq (toks "a;b;c; }"))  '((a  b c) })))

(7.3) Define a parser block

    <block> -> { <stmt_seq> }

    (example '(block (toks "{ b?x : y; }")  '((begin(if b x y))) )
    (example '(block (toks  "{ + 2 ; y; }")) #f )
    (example '(block (toks  "{ x + 2; y; }")) '((begin (+ x 2) y)))
    (example '(block (toks  "}")) #f)


[Hint - for the next 3 sections you can copy and adapt previous code for
arglist - alternatively you could generalise the code for arglist and
use it appropriately]

(8.1) Define a parser varlist


    <varlist> -> <identifier> , <varlist> | <identifier>


(example '(varlist (toks "x,y")) '((x y)))
(example '(varlist '(x y)) '((x) y))

(8.2) Define a parser <formals>

    <formals> -> ( <varlist> )

    (example '(formals (toks "(x,y)+3")) '((x y) + 3))
    (example '(formals (toks "(x,2)+3")) #f)
    (example '(formals (toks "x+3")) #f)

(8.3) Define a parser <formal_application>

   <formal_application> -> <identifier> <formals>

  (example '(formal_application (toks "fred(x,y) begin")) '((fred x y) begin))
  (example '(formal_application (toks "f(x,2)+3")) #f))
  (example '(formal_application (toks "f(x,y) begin")) '((f x y) begin)  )
  (example '(formal_application (toks "x+3")) #f)

(8.4) Define  a parser <typename>

    <typename> ->  int | float

(9.0) Define a parser <function>

   <function> -> <typename> <formal_application> <block>

  (example '(function
      (toks "int fred(x,y) { x+2*y;}"))
         '((define (fred x y) (begin (+ x (* 2 y)))))
       )


(10) Writing grammars using this kit of higher order functions is
straightforward but rather tedious. Suggest how you could apply this kit
to designing and implementing a language for making it easy to build parsers.
