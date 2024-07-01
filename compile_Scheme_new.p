

/*
compile_Scheme.p                     Robin Popplestone, March 1995

This is the main body of the POPLOG Scheme compiler.

/* (C) Copyright, University of Massachusetts, June 1994, All Rights Reserved
 *
 * This program carries no warranty.
 *
 * send bug reports and suggestions to pop@cs.umass.edu
 *

This program may reproduced for  academic and experimental purposes,  provided
the above attribution is preserved and extended as appropriate.

Commercial rights are reserved.

*/


Generate code for the Scheme language.

         CONTENTS - (Use <ENTER> g to access required sections)

 --  How the compiler works
 --    Notes on parameter passing
 --    Notes on booleans
 --  Lexical Analysis
 --    A Scheme atom is a POP-11 word with an extra space at the end.
 --    Schemify(v) converts a POP-11 object to scheme form
 --    tokenise handles the lexical analysis.
 --  read_sexpr is the main parser for Scheme
 --  compile_Scheme compiles a file or character repeater.
 --  LVARS_scm declares lexical locals with appropriate error message.
 --  Environments specify context for compilation.
 --  method maps from an atom to a procedure for compiling special forms.
 --  compile_expr(E,Env) compiles ordinary expressions
 --  compile_fun_call generates code for the function part of an expression.
 --  list_from_scm converts a Scheme list to POP-11 form.
 --  listify_count makes a list of arguments for variadic functions.
 --  compile_expr_seq(Seq,Env) plants code to handle a sequence of expressions.
 --  convert_bool converts a scheme object to a Poplog boolean.
 --  compile_bool plants code to compile boolean expressions.
 --  Special forms are treated by compile_... procedures
 --    compile_quote treats quoted expressions.
 --    compile_quasiquote treats quasi-quoted expressions.
 --    compile_lambda(E,Env) compiles lambda expressions.
 --      compile_args declares and pops the arguments of a function
 --    compile_define treats the (define ... ) construct
 --    compile_macro handles macro definitions
 --    compile_if(E,Env) compiles the if expression E.
 --    compile_and([and E1..En],Env) treats an "and" expression
 --    compile_or([or E1..En],Env) treats an "and" expression
 --    compile_case treats case statements
 --    compile_cond treats (cond ( (c1 v1) (c2 v2) ......(else vn))
 --  else is a reserved word.
 --    compile_begin handles the (begin E1...En) construct
 --    compile_do handles the do expression
 --    compile_let handles let and let* and letrec expressions.
 --     check_Binding checks the syntactic form of a binding  (v,E)
 --    compile_set_! treats the assignment statement in Scheme.
 --    compile_trace supports the tracing of function calls
 --  Setting up the standard bindings for built-in functions.
 --    display
 --    pr_scm is the general procedure to print a scheme object
 --    polyadic_from_binary(P,id) makes a scheme polyadic function
 --    The VAR macro allows us to bind Scheme variables to POP functions.
 --  SF(f,name) converts a POP-11 function f to a Scheme function with given name
 --  Errata.
 --    Missing or erroneous scheme functions.
 --    Pop-11 compatibility
 --    Lexical analysis
 --      Special forms not done
 --  ved menus
 --  Marking beginning and end of procedures in VED etc.
 --  The Scheme macro switches from POP-11

*/

'$popscheme'::popuseslist -> popuseslist;

uses lexical_args;
true -> popradians;

compile_mode:pop11 +defpdr +varsch +constr;
uses ved_source;

/*
declarations for GenericDebugger
*/

vars
  EditPanel_GDB,
  FileExt_default_GDB,
  HelpFile_GDB,
  HelpDebug_GDB,
;

vars procedure(
  Context_Env,
  is_symbol_GDB,
  pr_args_GDB,
  edit_GDB,
  tokenise_GDB,
  make_identifier_GDB,
  pr_proc_GDB,
  pr_GDB,
);

uses GenericDebugger;
vars procedure (mishap_scm = mishap_GDB(%"Expression_RT"%));
vars procedure (compile_debug = compile_debug_GDB);

define compile_debug_1(Name,Vars,Env, was_executing);
  unless was_executing then compile_debug_GDB(Name,Vars,Env)
  endunless;
enddefine;

define pr_args_GDB(Name,Args);
  lvars i;
  if isvector(Args) then
    pr("(");
    spr(Name);

    for i from 1 to datalength(Args)-1 do
      spr(subscrv(i,Args))
    endfor;
    pr(')\n');
  endif;
enddefine;

/*
How the compiler works
----------------------
This compiler is based on the use of the Poplog Virtual Machine. Since this is
conceptually a stack machine,  it is quite easy to generate scheme code.
Thus to compile the scheme expression  (f x1 x2) the simplest approach is
to push the values of x1 and x2 on the stack and enter the code for f.
If f, x1 and x2 are scheme atoms, then
   sysPUSH("x1");
   sysPUSH("x2");
   sysCALL("f");
is all that is required.  However this simple approach is complicated a
little by the need to keep scheme and POP-11 distinct, and by the handling
of scheme functions which can take a variable number of arguments as explained
below.

The main function of the compiler is -compile_expr-. Called in the form
compile_expr(Expr,Env) it checks to see whether the expression Expr is:-

    A variable: sysPUSH is called to generate code to push the value on the
        stack.
    A constant: sysPUSHQ is called to generate code to push the literal value.
    A function applied to argument: we use the property -method- to find a
        procedure to treat the function as a special form. If such a procedure
        exists, it is used to generate the code. If there is none such, we
        call compile_expr recursively to generate code to push the arguments,
        push a count of the arguments, and then call compile_fun_call to
        generate code to call the function.

compile_fun_call(f,Env) will, in the simplest case in which the function f
is an atom, uses sysCALL to generate code to enter the code for f. However
if f is a complex expression, it will call compile_expr to evaluate it, and
call sysAPPLY to plant code to execute the resulting function.

Most of the rest of the compiler serves to treat the special forms like
define, and, or, if, let. Also we have to provide a library of build-in
functions.

*/


/*
  Notes on parameter passing
-----------------------------
Scheme functions can be variadic, that is they take a variable number
of arguments. The actual number of arguments must be known unambiguously
when the function is called, but there is no way of knowing when
such a call is compiled whether or not the function which is called will
in fact be variadic. Thus, for example,  (f x y) may or may not be a call
of a variadic function.

In distinction, POP-11 procedures which take a variable number of arguments
take them off the stack according to conventions which must be obeyed by the
caller. For example, in  consstring(#| `c` `a` `t` |#) the user must know
that a variadic function is being called which expects an argument-count
on the stack and hence must use the #|...|# bracketing of the arguments or
the equivalent.

The convention adopted in this implementation of Scheme is that count of the
number of arguments is always put on the stack before the code for a function
is entered. This is the same as the default case for Poplog Common Lisp.
However, there is very little scope for optimisation in Scheme as it
is normally written, so the call-code is uniform, whereas in Poplog Common
Lisp it is often optimised out.

[Scheme global functions can always be redefined so

    (define (fred x) (+ x (* 3 y)))

cannot be optimised by supposing that + and * are the standard addition and
multiplication operators. We could optimise:

(define fred ( let ((+ +)(* *)) (lambda (x y) (+ x (* 3 y)))))

since the applications of + and * must be the values they had at definition
time, but this is hardly ever written!

  Notes on booleans
-------------------
Most LISPs, identify  the -false-  truth value  with the  empty list.  However
POPLOG, in common  with Scheme, has  a built-in -false-  object, which is  the
target of comparison for the conditional instructions of the Poplog Virtual
Machine. This is NOT the same as the empty list in POP-11.

*/


vars
  Env_init,
  nil_scm = [],
  pr_quotes_scm = true,
  undef_scm = 'Undefined Value',
  false_scm = false,
  lambda_explicit_?,       ;;; Controls putting definition in pdprops.
  show_macro_expansion_?,  ;;;
;

;;; Define record-classes for Scheme characters and files.

defclass _char {code_char:int};

defclass _file {name_file,rep_char_file,rep_token_file };

vars procedure(
    Add_to_Callers,
    applist_scm,
    assoc_scm,
    check_Binding,
    code_char,
    compile_args,
    compile_debug,
    compile_expr,
    compile_fun_call,
    compile_qq,
    cons_char,
    declare_Bindings,
    do_Bindings,
    exec_scm,
    first_function,
    is_char,
    is_symbol_scm,
    length_scm,
    make_Bindings,
    map_scm,
    method,
    mishap_scm = mishap_GDB(%"Expression_RT"%),
    mishap_wrong_args,
    pr_callers,
    pr_list_scm,
    pr_scm,
    pr_scm_n,
    push_Bindings,
    read_sexpr,
    read_to_close,
    rev_scm,
    setup_DebuggerPanel,
    setup_EditPanel,
    systrace_pr_scm,
);

/*
Lexical Analysis
----------------
*/

/*
  A Scheme atom is a POP-11 word with an extra space at the end.
----------------------------------------------------------------
*/

lconstant Symbol_flag_scm = consword(' ');
lconstant char_Symbol_flag_scm = Symbol_flag_scm(1);

/*
cadrify_list takes a list of characters forming the middle of an extended list
selector such as caddr and forms a Scheme expression to evaluate them.
*/

define cadrify_list(CharList);
    if null(CharList) then "x"
    elseif hd(CharList)==`a` then
        [%Schemify("car"),cadrify_list(tl(CharList))%]
    elseif hd(CharList)==`d` then
        [%Schemify("cdr"),cadrify_list(tl(CharList))%]
    else
        false
    endif;
enddefine;

/*
cadrify takes a scheme Symbol and, if it is the name of an extended list selector
such as caddr, it generates and compiles a definition of it.
*/

define cadrify(Symbol);
    let n = datalength(Symbol) in
        if n>4 then
            if subscrw(1,Symbol) == `c` and subscrw(n-1,Symbol) == `r` then
                let Body = cadrify_list(
                        datalist(allbutfirst(1,allbutlast(2,Symbol))))
                in
                    if Body then
                        compile_expr(
                            [%define_scm,
                                [%Symbol, "x"%],
                                Body
                            %],
                            Env_init)
                    endif
                endlet;
            endif;
        endif;
        Symbol
    endlet;
enddefine;

;;;trace cadrify;

/*
trace compile_expr;
cadrify(Schemify("cadadar")) =>
cadrify(Schemify("car"))=>
*/

define mk_atom_scm(WS);
    let Symbol =
        if isstring(WS) then
            consword(uppertolower(WS))<>Symbol_flag_scm
        else
            uppertolower(WS)<>Symbol_flag_scm
        endif
    in
        if identprops(Symbol) = undef then
            cadrify(Symbol)
        else
            Symbol
        endif;
    endlet
enddefine;

/*
(pop11)
mk_atom_scm("cadaddar")=>
*/

vars true_scm = true;


example mk_atom_scm;
  mk_atom_scm("Fred"),";" =>
** fred  ;
  isword(mk_atom_scm('null?')) =>
** <true>
endexample;

define is_symbol_scm(Obj);
  isword(Obj) and last(Obj) == char_Symbol_flag_scm
enddefine;

is_symbol_scm -> is_symbol_GDB;

example is_symbol_scm;
  is_symbol_scm(34)=>
** <false>

  is_symbol_scm("fred")=>
** <false>

  is_symbol_scm(consword('fred '))=>
** <true>

endexample

/*
  Schemify(v) converts a POP-11 object to scheme form
------------------------------------------------------
A Scheme function takes one more argument than the corresponding POP-11
procedure in order to support the Scheme convention for passing a variable
number of arguments.
*/

lvars Msg_wrong_args =
  'Calling function \'%p\' with wrong number (%p) of arguments, it needs %p';

vars nil_scm = [];

define caller_scm();
;;;  syscallers()=>
  caller(1);
enddefine;


compile_mode:pop11 +defpdr +varsch +constr;

define Schemify_function(v,name);
    lvars m = pdnargs(v);                 ;;; how many arguments does it need?
    lvars Val =
      if m==0 then
        procedure(/*..*/,n)with_nargs 1;      ;;; return a procedure which
          unless n==m then                    ;;; checks that the top of stack
            mishap_scm(Msg_wrong_args,     ;;; is the same as the expected
              [%name,n,m%]);         ;;; number of arguments.
          endunless;
          v(/*...*/);                       ;;; before calling the original one
        endprocedure;
       elseif m ==1 then
       procedure(/*..*/,n)with_nargs 2;      ;;; return a procedure which
      unless n==m then                    ;;; checks that the top of stack
        mishap_scm(Msg_wrong_args,     ;;; is the same as the expected
                      [%name,n,m%]);         ;;; number of arguments.
      endunless;
        v(/*...*/);                       ;;; before calling the original one
;;;        v->v;                             ;;; needed for error rept.
    endprocedure;
    elseif m==2 then
    procedure(/*..*/,n)with_nargs 3;      ;;; return a procedure which
      unless n==m then                    ;;; checks that the top of stack
        mishap_scm(Msg_wrong_args,     ;;; is the same as the expected
                      [%name,n,m%]);         ;;; number of arguments.
      endunless;
        v(/*...*/);                       ;;; before calling the original one
    endprocedure;

    elseif m==3 then
    procedure(/*..*/,n)with_nargs 4;      ;;; return a procedure which
      unless n==m then                    ;;; checks that the top of stack
        mishap_scm(Msg_wrong_args,     ;;; is the same as the expected
                      [%name,n,m%]);         ;;; number of arguments.
      endunless;
        v(/*...*/);                       ;;; before calling the original one
    endprocedure;

    elseif m==4 then
    procedure(/*..*/,n)with_nargs 5;      ;;; return a procedure which
      unless n==m then                    ;;; checks that the top of stack
        mishap_scm(Msg_wrong_args,     ;;; is the same as the expected
                      [%name,n,m%]);         ;;; number of arguments.
      endunless;
        v(/*...*/);                       ;;; before calling the original one
    endprocedure;

    else mishap('too many args of proc being schemified',[^v]);
    endif;

    'Scheme function: ' >< pdprops(v) >< '(' >< m ><')' ->
    pdprops(Val);
    Val
enddefine;

define Schemify(v);
  if isprocedure(v) then                  ;;; if it is a procedure
    Schemify_function(v,false);
  elseif ispair(v) then
    conspair(Schemify(front(v)),
               Schemify(back(v)))
  elseif isword(v) then mk_atom_scm(v);
  elseif v==[]  then nil_scm
  else v                                  ;;; otherwise use POP-11 value
  endif;
enddefine;

Schemify -> make_identifier_GDB;

vars procedure(
  make_token,
  skip_comment,
  stack_string,
  tokenise,

);

lvars i;
vars chartype = inits(128);


;;; 0 = whitespace, 1 = identifier, 2 = number, 3 = delimiter, 5=error

for i from 1 to 128 do 1 -> chartype(i); endfor;
for i from 1 to 31 do 5 -> chartype(i);
endfor;

0 -> chartype(`\t`);
0 -> chartype(`\n`);
0 -> chartype(` `);



for i from `0` to `9` do 2 -> chartype(i)
endfor;

2 -> chartype(`.`);  ;;; can appear in a number
2 -> chartype(`+`);
2 -> chartype(`-`);

;;; Delimiters. Certain of these are recognised per se and acted on
;;; accordingly.


3 -> chartype(`(`);
3 -> chartype(`)`);

3 -> chartype(`;`);  ;;; comment
3 -> chartype(`"`);  ;;; String quote
3 -> chartype(`\'`); ;;; S-expression quote
3 -> chartype(`\``);
3 -> chartype(`|`);

3 -> chartype(`{`);
3 -> chartype(`}`);

3 -> chartype(`[`);
3 -> chartype(`]`);

/*
  tokenise handles the lexical analysis.
-----------------------------------------
This is not yet correct. In particular POP-11 conventions for numbers
are used. We assume that a token is a number unless proved otherwise.
This is because a single exception can turn a number into a non-number.
However, our initial filter for numbers is crude, and merely serves
for efficiency. The variable "type" holds the presumed type of the
token.

We begin by scanning past any blank-space (1). The lexical analyser
uses an "overshoot" character c_prev to allow for the necessity of looking
ahead.

Having found the beginning of a token we stack up its characters (2).

;;; 0 = whitespace, 1 = identifier, 2 = number, 3 = delimiter, 5=error
*/

lconstant Msg_wrong_char =
        'Illegal character "%p" in input file %p';

define tokenise(Rep);
    lvars c_prev = false;                   ;;; overshoot character from prev
    procedure()->tok;
        lvars c, tok,type = "number";

        repeat                                ;;; (1) Scan blanks
            if c_prev then                      ;;; use overshoot character?
                c_prev, false -> c_prev
            else
                Rep()
            endif -> c;
            ;;;printf('scanning blanks: c=%p,c_prev = %p\n',[^c^c_prev]);
            if c==termin then return(c->tok);
            endif;
            if c == `;` then
                skip_comment(Rep); `\n`->c;
            endif;
        quitif ((subscrs(c,chartype)) /== 0);         ;;; get syntactic type
        endrepeat;
        make_token(#|                       ;;; (2) read token.
                repeat
                    if c==`;` then                ;;; comment ends a token.
                        skip_comment(Rep);
                        if c_prev then
                            `\n` -> c_prev;
                            quitloop
                        else Rep()->c;
                        endif;
                    endif;
                    ;;;printf('making token: c=%p\n',[^c]);
                    lvars t = subscrs(c,chartype);
                quitif(t==0);                    ;;; whitespace not incorporated
                    if t==5 then mishap_scm(
                            Msg_wrong_char,
                            [%c,Rep%]);
                    endif;
                    if c==`"` then
                        stack_string(Rep);
                        "string" -> type;
                        quitloop;
                    endif;

                    if t/==2 then
                        if type == "number" and c==`/` then
                            "rational" -> type;
                        else
                            "identifier" -> type       ;;; Cannot be a number.
                        endif
                    endif;
                    c;
                    if c==`,` then Rep() -> c;   ;;; ,@ sticks together
                        if c == `@` then c
                        else c -> c_prev
                        endif ;
                        quitloop;
                    endif;
                    if c==`#` then Rep() -> c;   ;;; deal with # forms
                        if c == `(` or c == `f` or c == `t` or c == `\\` then c
                        else c -> c_prev
                        endif ;
                        if c/== `\\` then quitloop;
                        else  Rep() -> c;
                            unless isalphacode(c) then c; quitloop
                            endunless;
                            c
                        endif;

                    endif;
                quitif(t==3);                    ;;; Delimiter is complete as 1 char
                    Rep()-> c;
                    c->c_prev;
                    ;;;printf('new char = %p\n',[^c]);
                quitif( subscrs(c,chartype) == 3);
                endrepeat |#,type)

            -> tok;
        ;;;      dlocal pop_pr_quotes = true; spr(tok);
    endprocedure
enddefine;

tokenise -> tokenise_GDB;

/*
Put the characters of a string on the stack.
*/

define stack_string(Rep);
  lvars c;
  repeat Rep()->c;
     if c==termin then mishap_scm('Unterminated string',[]);
     endif;
     quitif(c=`"`);
     if c==`\\` then
        Rep()->c;
        if c==`n` then
           `\n`
        elseif c==`"` or c == `\\` then c
        endif -> c;
     endif;
     c;
  endrepeat;
enddefine;

/*
make_token takes the stacked up characters which constitute a Scheme
token and converts them into the appropriate object.  type indicates
the type of object that is to be created.

??? The full scheme syntax for a number is not implemented.
*/

lconstant Name_char_space = consword('#\\space');
lconstant Name_char_newline = consword('#\\newline');
lconstant Name_char_tab = consword('#\\tab');

define make_token(/*...*/ type) with_nargs 3;
    if type=="number" then                      ;;; Did it look like a number?
        lvars Str = consstring(/*..*/);
        lvars n   = strnumber(Str);
        if n then return( n)
        else
            deststring(Str)                  ;;; Was not a number, make an atom.
        endif;
    elseif type=="string" then
        return(consstring(/*..*/));
    endif;
    lvars Word = consword(/*...*/);
;;;    [%Word, Name_char_space,datalist(Word),
;;;            datalist(Name_char_space),Word==Name_char_space%] =>
    if subscrw(1,Word) == `#` then
        if datalength(Word) == 2 then
            if subscrw(2,Word) == `f` then
                false -> Word
            elseif subscrw(2,Word) == `t` then
                true -> Word
            endif;
        elseif datalength(Word) == 3 and subscrw(2,Word) = `\\` then
             cons_char(subscrw(3,Word)) -> Word;
        elseif Word == Name_char_space then cons_char(` `) -> Word;
        elseif Word == Name_char_newline then cons_char(`\n`) -> Word;
        elseif Word == Name_char_tab then cons_char(`\t`) -> Word;
        else mishap_scm('Unknown character name %p', [%Word%]);
        endif
    endif;
    Schemify(Word)
enddefine;


define skip_comment(Rep);
  until Rep() == `\n` do
  enduntil;
enddefine;


/*
read_sexpr is the main parser for Scheme
---------------------------------------
*/

lconstant Comma_scm = Schemify(",");
lconstant CommaAt_scm = Schemify(consword(',@'));


lconstant quote = consword('\'');
lconstant quote_scm = Schemify("quote");

lconstant unquote_scm = Schemify("unquote");
lconstant unquote_splicing_scm = Schemify(consword('unquote-splicing'));

lconstant Quote_scm = Schemify(quote);
lconstant quasiquote_scm = Schemify("quasiquote");
lconstant QuasiQuote_scm = Schemify("'\`'");
lconstant OpenParen_scm = Schemify("(");
lconstant OpenVector_scm = Schemify("'#('");
lconstant CloseParen_scm = Schemify(")");
lconstant Period_scm     = Schemify(".");

lconstant Msg_no_line_num = 'from terminal';


define conspair_ln(x,y);
    let p = conspair(x,y),
        n = if poplinenum then poplinenum else Msg_no_line_num endif
    in
        n -> index_ln_GDB(p);
        ;;;     printf('Indexing %p at line %p\n', [^p^poplinenum]);
        p
    endlet;
enddefine;


define vectorify = destlist<>consvector;
enddefine;

define read_sexpr(read_item);
  lvars item = read_item();

  if item == OpenParen_scm then read_to_close(read_item);

  elseif item == OpenVector_scm then
    vectorify(read_to_close(read_item))

  elseif item == Quote_scm then
     conspair_ln(quote_scm,conspair(read_sexpr(read_item),nil_scm))

  elseif item == QuasiQuote_scm then
     conspair_ln(quasiquote_scm,conspair(read_sexpr(read_item),nil_scm))

  elseif item == Comma_scm then
     conspair_ln(unquote_scm,conspair(read_sexpr(read_item),nil_scm))

  elseif item == CommaAt_scm then
     conspair_ln(unquote_splicing_scm,conspair(read_sexpr(read_item),nil_scm))

  else item
  endif;
enddefine;

vars  Msg_end_in =
      'End of input encountered while reading Scheme expression,'
    <> 'probably a missing close parenthesis or "';


define read_to_close(read_item);
  lvars item = read_item();
  if item == CloseParen_scm then nil_scm
  elseif item == OpenVector_scm then
    conspair_ln(vectorify(read_to_close(read_item)),
                read_to_close(read_item))
  elseif item == OpenParen_scm then
    conspair_ln(read_to_close(read_item),read_to_close(read_item))

  elseif item == Quote_scm then
     conspair_ln(
         conspair(quote_scm,conspair(read_sexpr(read_item),nil_scm)),
         read_to_close(read_item))
  elseif item == QuasiQuote_scm then
     conspair_ln(
         conspair(quasiquote_scm,conspair(read_sexpr(read_item),nil_scm)),
         read_to_close(read_item))

  elseif item == Comma_scm then
     conspair_ln(
         conspair(unquote_scm,conspair(read_sexpr(read_item),nil_scm)),
         read_to_close(read_item))

  elseif item == CommaAt_scm then
     conspair_ln(
         conspair(unquote_splicing_scm,conspair(read_sexpr(read_item),nil_scm)),
         read_to_close(read_item))
  elseif item == Period_scm then
    lvars item = read_item();
    lvars item_end = read_item();
    unless item_end == CloseParen_scm then
       mishap_scm('Improperly formed dotted list . %p %p',[^item ^item_end])
    endunless;
    item;
  elseif item == termin then
    mishap_scm(
      Msg_end_in,
      []);
  else conspair_ln(item,read_to_close(read_item));
  endif;
enddefine;

;;;lconstant apply_pair = class_apply(pair_key);

define mishap_apply_list(item);
   mishap_scm('cannot apply list \'%p as a function',[%item%])
enddefine;

/*
compile_Scheme compiles a file or character repeater.
----------------------------------------------------
*/


define compile_Scheme(Rep);

    dlocal popfilename;
    if isstring(Rep) then Rep
    else vedpathname
    endif -> popfilename;
;;;    printf('popfilename set to %p in compile_Scheme',[^popfilename]);

    if isstring(Rep) or isdevice(Rep) then discin(Rep) -> Rep
    endif;
    lvars read_item        = tokenise(Rep);
    dlocal cucharin = Rep;
    dlocal depth_max_GDB = 1000000;
    dlocal expr_now;
    dlocal pop_longstrings = true;
    dlocal popprompt = '=> ';
;;;    dlocal %class_print(boolean_key)% = pr_scm;
;;;    dlocal %class_print(word_key)% = pr_scm;
    dlocal %class_print(undef_key)% = pr_scm;
    dlocal %class_print(procedure_key)% = pr_scm;
    dlocal %class_print(pair_key)%       = pr_scm;
    dlocal systrace_pr                   = systrace_pr_GDB;
    dlocal index_ln_GDB = newassoc([]);

;;;    dlocal %class_apply(pair_key)%      = mishap_apply_list;

    while true do
        lvars E = read_sexpr(read_item);
    quitif(E=termin);
        #_IF identprops("ts_Scheme") == 0
            ts_Scheme(E)
        #_ENDIF
        compile_expr(E,Env_init);
        lvars Val = exec_scm();
        pr(newline);
        pr_scm(Val); pr(newline);
    endwhile;
enddefine;

define schemecompile(file);       ;;; This name needed for VED
  sysCOMPILE(file, compile_Scheme)
enddefine;
/*
LVARS_scm declares lexical locals with appropriate error message.
----------------------------------------------------------------
*/

lconstant msg_wrong_var =
  'trying to declare expression %p as variable';
lconstant Msg_reserved_id =
      'Trying to use the reserved symbol \'%p as variable';

define LVARS_scm(V);
  if isword(V) then
     if method(V) then
        mishap_scm(Msg_reserved_id,
                [^V]);
     else
        sysLVARS(V,0)
     endif;
  else
     mishap_scm(msg_wrong_var,[%V%]);
  endif
enddefine;


/*
Environments specify context for compilation.
----------------------------------------------
Given the support provided by the Poplog VM, little is needed in the way
of an environment.  However we do need to remember whether we are in
a local or global context, since scheme top-level variables need to be
treated as Poplog globals.  It is also handy to tag lambda expressions
with a name derived from the nearest enclosing function created by (define..)

Also we remember in the environment whether we are generating POP-11 compatible
code.
*/

defclass _Env{
  Context_Env,
  TypeInfo_Env};

;;; make a local version of an environment.

define local_Env(Name,Env)->Env_new;
   copy(Env) -> Env_new;
   Name -> Context_Env(Env_new);
enddefine;

define TypeMap_global = newassoc(
[[ %Schemify("+") % [-> [number number] number]]]);
enddefine;

vars Env_init = cons_Env(false,TypeMap_global);


define Type_var(V,Env);
  TypeInfo_Env(Env)(V)
enddefine;

/*
method maps from an atom to a procedure for compiling special forms.
--------------------------------------------------------------------
*/

vars procedure (
  args   = back,
  method = newassoc([]),

);

/*
The updater of method takes a POP-11 atom and converts it into
the corresponding scheme atom with which a procedure is associated.
*/

lvars updater_method = updater(method);

define updaterof method(Val,Atom);
  lvars A = mk_atom_scm(Atom);
  updater_method(Val,A);
enddefine;


define is_var(E);
    if isword(E) then
        if method(E) then
            mishap_scm(Msg_reserved_id,[^E]);
        else
            true
        endif;
    else
        false
    endif;
enddefine;

vars
Type_number    = [number],
Type_procedure = [procedure],
Type_string    = [string],
Type_boolean   = [boolean],
Type_top       = [number procedure boolean string],
;

define Type_constant(E);
    if      isnumber(E)     then Type_number
    elseif  ispair(E)       then Type_constant_list(E)
    elseif  isprocedure(E)  then Type_procedure(E)
    elseif  isstring(E)     then Type_string
    elseif  isboolean(E)    then Type_boolean
    else    Type_top
    endif;
enddefine;


/*
compile_expr(E,Env) compiles ordinary expressions
-------------------------------------------------
*/


define compile_expr(E,Env);
    if is_var(E) then                     ;;; a variable? push its value
        sysPUSH(E);
            (Type_var(E,Env),
            Env)
    elseif atom(E) then
        (sysPUSHQ(E),
            Type_constant(E),
            Env
        )                                 ;;; a constant? push it itself
    else                                  ;;; a function applied to arguments
        lvars
            f         = front(E),            ;;; get the function
            compile_f = method(f);           ;;; is it a special form (e.g. if)?

        if compile_f then                   ;;; use the appropriate method
            compile_f(E,Env)
        else                                ;;; not a special form
            let
                Name = Context_Env(Env),
                Name_1 =
                if Name and issubstring('<lambda',Name)
                then Name
                else consword('<lambda in '><Name><">")
                endif,

                Env_i = local_Env(Name_1,Env)
            in
                dlocal Context_err = E;
                let E_i, Ty_args =
                    [%  for E_i in args(E) do
                            compile_expr(E_i,Env_i) -> (Ty_i,Env_i);
                            Ty_i;
                        endfor
                    %]
                in                                        ;;; push the arguments on the stack
                    sysPUSHQ(length_scm(E)-1);            ;;; push the number of arguments
                    compile_fun_call(f,Ty_args,Env_i);    ;;; call the function.
                endlet
            endlet
        endif;
    endif;
    let i = index_ln_GDB(E) in
        if debugging_? and Context_Env(Env)     ;;; Record the line for the
        and i                                   ;;; debugger.
        and not(popexecute)
        then
            sysPUSHQ(E);
            sysPUSHQ(i);
            sysCALL("breakpoint_GDB");
        endif;
    endlet
enddefine;

/*
compile_fun_call generates code for the function part of an expression.
----------------------------------------------------------------------
??? Do we need the isprocedure check???
*/

define apply_checking(f);
  if isprocedure(f) then f(/*...*/)
  else  mishap_scm('Cannot apply object %p as function',[^f]);
  endif;
enddefine;

define compile_fun_call(f,Ty_args,Env);
    if is_var(f) then                             ;;; A variable?
        let Ty_f = Type_var(f,Env)
        in
            if debugging_? then
                sysPUSH(f);
                sysCALLQ(apply_checking);
            else
                sysCALL(f);                           ;;; call indirectly thro' f
            endif;
            Type_call(Ty_f,Ty_args,Env);
        endlet;
    elseif isprocedure(f) then                    ;;; a function?
        sysCALLQ(f);                              ;;; call directly
        Top_Type;
    elseif atom(f) then
        mishap_scm(
            'cannot apply %p as function',
            [%f%]);
    else                                        ;;; f is a complex expression
        let (Ty_f,Env_f) = compile_expr(f,Env)    ;;; evaluate it
        in
            sysCALLQ(apply_checking);
            Type_call(Ty_f,Ty_args,Env);
        endlet;
    endif;
enddefine;


define Type_call(Ty_f,Ty_args,Env);
    if Ty_f then

        is_fun_Type(Ty_f) then
        mishap('function type needed',[

        else

        endif;
    enddefine;



/*
list_from_scm converts a Scheme list to POP-11 form.
----------------------------------------------------
*/

define list_from_scm = identfn
enddefine;

/*
listify_count makes a list of arguments for variadic functions.
---------------------------------------------------------------
*/

define listify_count(/*..*/,n)->List with_nargs 3;
  lvars List = nil_scm;
     repeat n times  /*x_i*/ :: List -> List
     endrepeat;
enddefine;

/*
compile_expr_seq(Seq,Env) plants code to handle a sequence of expressions.
-----------------------------------------------------------------------
Since the value of each expression is pushed on the stack, we must remove
all but the last one.

MIT scheme does not allow an empty expression sequence.

*/


define compile_expr_seq(Seq,Env);
    if atom(Seq) then
        mishap_scm('wrong form for expression sequence %p',
            [^Seq]);
    else
        let Ty_i,Env_i = Env
        in
            while not(atom(Seq)) do
                compile_expr(front(Seq),Env) -> (Ty_i,Env_i); ;;; evaluate the expression

                back(Seq) -> Seq;
                unless atom(Seq) then
                    sysERASE(false);             ;;; pop the stacked value if not last
                endunless;
            endwhile;
            (Ty_i,Env_i)                         ;;; Type of sequence is type
        endlet                                   ;;; of last item.
    endif;
enddefine;

define compile_expr_seq_0(Seq,Env);
  if Seq==nil_scm then sysPUSHQ(undef_scm); (Type_undef,Env);
  else  compile_expr_seq(Seq,Env);
  endif;
enddefine;


/*
convert_bool converts a scheme object to a Poplog boolean.
----------------------------------------------------------
*/

define convert_bool = identfn;
enddefine;

/*
compile_bool plants code to compile boolean expressions.
--------------------------------------------------------
*/

define compile_bool(E,Env);
    let (Ty,Env1) =   compile_expr(E,Env)
    in
        unless Compatible(Ty,Type_boolean) then
            Warn('Expression %p cannot produce boolean result',[^E])
        endunless;
        (Ty,Env1);
    endlet;
enddefine;

/*
Special forms are treated by compile_... procedures
---------------------------------------------------
*/


/*
  compile_quote treats quoted expressions.
------------------------------------------
*/

define compile_quote(E,Env);
   unless length_scm(E)==2 then
     mishap_scm('Wrong form for quoted expression "%p"', [^E]);
   endunless;
   sysPUSHQ(front(back(E)));
   Type_constant(front(back(E)));
enddefine;

compile_quote -> method("quote");



/*
  compile_quasiquote treats quasi-quoted expressions.
------------------------------------------
*/

define compile_quasiquote(E,Env);
   unless length_scm(E)==2 then
     mishap_scm('Wrong form for quasi-quoted expression "%p"', [^E]);
   endunless;
   compile_qq(front(back(E)),Env);
enddefine;

compile_quasiquote -> method("quasiquote");


define compile_qq(E,Env);
  if atom(E) then sysPUSHQ(E)

  elseif front(E) == unquote_scm then
    compile_expr(front(back(E)),Env);

  elseif not(atom(front(E))) and                 ;;; ((unquote-splicing f) g)
     front(front(E)) == unquote_splicing_scm
  then
    compile_expr(front(back(front(E))),Env);
    compile_qq(back(E),Env);
    sysCALLQ(nonop <>);
  else compile_qq(front(E),Env); compile_qq(back(E),Env);
       sysCALLQ(cons);
  endif;
  Type_general_list(),Env;
enddefine;



/*
  compile_lambda(E,Env) compiles lambda expressions.
----------------------------------------------------
*/

define tidy_name(Name);
 ;;;allbutlast(1,Name)
 Name;
enddefine;


define compile_lambda(E,Env);
    dlocal Context_err = E;
    /* Do not set Context_err here */
    lvars Args    = front(back(E));
    lvars Body    = back(back(E));
    lvars Name    = tidy_name(Context_Env(Env));
    lvars
        Ty_args,
        Ty_body;
    if islist(Args) then                   ;;; Fixed number of arguments?
        sysPROCEDURE(Name, 1+length_scm(Args));
        if debugging_? then
            sysLOCAL("chain_callers");
            sysPUSHQ(Name);
            sysCALLQ(Add_to_Callers);
        endif;
        lvars l_ok_args = sysNEW_LABEL();     ;;; Yes! - check that correct
        sysPUSHQ(length_scm(Args));              ;;; number is passed.
        sysCALLQ(nonop ==);
        sysIFSO(l_ok_args);
        sysPUSHQ(Name);
        sysCALLQ(mishap_wrong_args);
        sysLABEL(l_ok_args);
        compile_args(Args,Env);              ;;; plant code to unstack args.
        compile_debug(Name,Args,Env);
    else                                   ;;; Variable number of args
        sysPROCEDURE(Name,2);
        if debugging_? then
            sysLOCAL("chain_callers");
            sysPUSHQ(Name);
            sysCALLQ(Add_to_Callers);
        endif;
        sysCALLQ(listify_count);             ;;; make into list.
        let L_args = [%Args%] in
            compile_args(L_args,Env);          ;;; and treat as one argument.
            compile_debug(Name,L_args,Env)
        endlet
    endif -> Ty_args;
    compile_expr_seq(Body,
        cons_Env(consword('<lambda in '><Context_Env(Env)><">"),
            is_pop_compat_Env(Env))) -> Ty_body;
    lvars Code =   sysENDPROCEDURE();
    if lambda_explicit_? and not(Context_Env(Env))
    then
        sysPUSHQ(E);
        sysPUSHQ(Code);
        sysCALLQ(updater(pdprops))
    endif;
    sysPUSHQ(Code);
    if isprocedure(source_of_proc) and debugging_? then
    sysPUSHQ(vedpathname);
    sysPUSHQ(Name);
    sysUCALLQ(source_of_proc);
    endif;
    Type_function(Ty_args,Ty_body);
enddefine;

compile_lambda -> method("lambda");

/*
check_duplicate ensures that variables are unique in argument lists.
*/

define check_duplicate(Args,Atom,Env);
  dlocal Context_err_local = Args;
  dlocal Class_err_local = 'argument list';
  lvars Args1;
  for Args1 on Args do
    lvars Arg = front(Args1);
    if member(Arg,back(Args1)) then
       mishap_scm(
         '%p expression has repeated argument "%p" in "%p"',
         [%Atom,Arg,Args%]);
    endif;
 endfor;
enddefine;

/*
    compile_args declares and pops the arguments of a function
--------------------------------------------------------------
*/

define compile_args(Args,Env);           ;;;
  lvars Arg;
  check_duplicate(Args,"lambda",Env);
  for Arg in  rev_scm(Args) do            ;;; We pop them in reverse order
    LVARS_scm(Arg);                       ;;; declare var as lexical local
    sysPOP(Arg);                         ;;; pop it.
    Type_variable();
  endfor;
enddefine;

/*
  compile_define treats the (define ... ) construct
----------------------------------------------------
There are two cases, exemplified by
  (define var  27)
and
  (define (fred x y) (+ x y))

*/

;;; lconstant
vars
    define_scm = Schemify("define"),
    lambda_scm = Schemify("lambda"),
    else_scm   = Schemify("else"),
    let_scm = Schemify("let"),
    letrec_scm = Schemify("letrec");


lconstant msg_need_var =
  'Need variable name or expression instead of "%p" in definition "%p"';



define compile_define(E,Env);
;;;    dlocal debugging_? = false;
    lvars n = length_scm(E);
    if n <= 2 then
        mishap_scm('define statement "%p" defines nothing',[^E]);
    endif;
    lvars Var = front(back(E));
    dlocal Context_err_def = Var;
    if method(Var) then
        mishap_scm('Attempting to redefine reserved word \'%p',[^Var]);
    endif;
    if is_var(Var) then                         ;;; first case.
        if n/==3 then
            mishap_scm(
                'too many forms in define statement:\n%p\n ',
                [%E%]);
        endif;

        if Context_Env(Env) then           ;;; Nested variables are lexical
            LVARS_scm(Var);
        else                               ;;; Top-level variables are non-lexical.
            sysVARS(Var,0);
            popfilename -> source_of_proc(Var);
        endif;
        compile_expr(front(back(back(E))),
            local_Env(Var,Env));   ;;; Get value of expression.
        sysPUSHS(false);
        sysPOP(Var);                        ;;; and pop it to the variable.
    elseif atom(Var) then
        mishap_scm(msg_need_var,[^Var^E]);

    else                                  ;;;  2nd case - convert to 1st
        let  Args = back(Var),
            Body = back(back(E)),
            Var1 = front(Var),
            LExpr =  conspair_ln(          ;;; enter it index for line-numbers
                         lambda_scm,
                         [ ^Args ^^Body]),
        in
            compile_define(
                [define ^Var1 ^LExpr],
                Env)
        endlet
    endif;
enddefine;

compile_define -> method("define");

/*
  compile_macro handles macro definitions
-----------------------------------------

Macros are handled by using convert_to_method to connvert a scheme function
to a compile-method.

*/

define convert_to_method(F);
    lvars F;
    procedure(E,Env);
      pr('expanding macro: '); pr_scm(F);
      pr('\nfor expression: '); pr_scm(E); pr(newline);
      lvars E1 = F(E,1);
      printf('expands to %p\n',[^E1]);
      compile_expr(E1,Env);
    endprocedure;
enddefine;

define compile_macro(E,Env);
   dlocal Context_err = E;
   lvars Binding = back(E);
   check_Binding(Binding,Env);
   compile_expr_seq(back(Binding),Env);
   sysCALLQ(convert_to_method);
   sysPUSHQ(allbutlast(1,front(Binding)));
   sysCALLQ(updater(method));
   sysPUSHQ(undef_scm);
enddefine;

compile_macro -> method("macro");

/*
  compile_if(E,Env) compiles the if expression E.
-------------------------------------------------
*/


define compile_if(Expr,Env);
  dlocal Context_err = Expr;
  lvars n = length_scm(Expr);
  if n==3 then
    Expr<> [^undef_scm] -> Expr;
    4->n;
  endif;
  unless n == 4 then
    mishap_scm('Badly formed "if" expression %p',[%Expr%])
  endunless;
  lvars (_,C,E_ifso,E_ifnot) = explode(Expr);
  compile_bool(C,Env);
  lvars lab_ifnot = sysNEW_LABEL();
  lvars lab_done  = sysNEW_LABEL();
  sysIFNOT(lab_ifnot);
  compile_expr(E_ifso,Env);
  sysGOTO(lab_done);
  sysLABEL(lab_ifnot);
  compile_expr(E_ifnot,Env);
  sysLABEL(lab_done);
enddefine;

compile_if -> method("if");

/*
  compile_and([and E1..En],Env) treats an "and" expression
----------------------------------------------------------
*/

define compile_and(Expr,Env);
    dlocal Context_err = Expr;
    lvars Arg,Args = back(Expr);
    if null(Args) then
    else
        lvars lab_false = sysNEW_LABEL();
        lvars lab_true  = sysNEW_LABEL();
        compile_expr(front(Args),Env);
        for Arg in  list_from_scm(back(Args)) do
            sysAND(lab_false);
            compile_bool(Arg,Env);
        endfor;
        sysGOTO(lab_true);
        sysLABEL(lab_false);
        sysLABEL(lab_true);
    endif;
enddefine;

compile_and -> method("and");
/*
  compile_or([or E1..En],Env) treats an "and" expression
----------------------------------------------------------
*/

define compile_or(Expr,Env);
    dlocal Context_err = Expr;
    lvars Arg,Args = back(Expr);
    if null(Args) then
    else
        lvars lab_false = sysNEW_LABEL();
        lvars lab_true  = sysNEW_LABEL();
        compile_bool(front(Args),Env);
        for Arg in  list_from_scm(back(Args)) do
            sysCALLQ(convert_bool);
            sysOR(lab_true);
            compile_bool(Arg,Env);
        endfor;
        sysGOTO(lab_false);

        sysLABEL(lab_true);
    ;;;    sysPUSHQ(true_scm);

        sysLABEL(lab_false);
    endif;
enddefine;

compile_or -> method("or");


/*
  compile_case treats case statements
-----------------------------------------------------
This is done much like cond.
*/

lvars Msg_wrong_case =
  'Wrong form for key-value pair "%p" in (case ...) expression\n%p';

define compile_case(Expr,Env);
  dlocal Context_err = Expr;
  lvars CV, was_else = false;
  unless length(Expr) > 2 then
    mishap_scm('case expr "%p" needs actual cases', [^Expr]),
  endunless;
  lvars Target     = front(back(Expr));
  lvars Var        = sysNEW_LVAR();
  lvars List_CV = back(back(Expr));             ;;; get list of case-value pairs
  lvars lab_next = sysNEW_LABEL();       ;;; where to go for the next CV pair
  lvars lab_done = sysNEW_LABEL();       ;;; where to go when evaluation is done
  compile_expr(Target,Env);
  sysPOP(Var);
  for CV in list_from_scm(List_CV) do   ;;;
     if atom(CV) then                   ;;; Check correct form.
          mishap_scm(Msg_wrong_case,
                        [^CV ^Expr]);
     endif;
    lvars C = front(CV);                   ;;; Get the case.
    sysLABEL(lab_next);                 ;;; come here to try new case.
    sysNEW_LABEL() -> lab_next;          ;;; and go here if you fail.
    if C == else_scm then                  ;;; else? do it unconditionally.
      true -> was_else;
      compile_expr_seq(back(CV),Env)
    else                                ;;; otherwise we must do a test.
      sysPUSH(Var);
      sysPUSHQ(C);
      sysCALLQ(member);
      sysIFNOT(lab_next);               ;;; test fails? try next pair
      compile_expr_seq(back(CV),Env);         ;;; succeeds? get the value
      sysGOTO(lab_done);                ;;; and go to the end
    endif;
  endfor;
  sysLABEL(lab_next);
  unless was_else then
    sysPUSHQ(undef_scm);
  endunless;
  sysLABEL(lab_done);                   ;;; whole expression complete.
enddefine;

compile_case -> method("case");




/*
  compile_cond treats (cond ( (c1 v1) (c2 v2) ......(else vn))
--------------------------------------------------------------
The code generated for this expression can be explained as follows:

lab_next_1: evaluate c1;  false? goto lab_next_2 ; evaluate v1; goto lab_done
lab_next_2: evaluate c2;  false? goto lab_next_3 ; evaluate v2; goto lab_done

lab_next_n:  evaluate vn;
lab_done:


*/

lvars Msg_wrong_cond =
  'Wrong form for condition-value pair "%p" in (cond ...) expression %p';
;;; null expression sequence ....

define compile_cond(Expr,Env);
  dlocal Context_err = Expr;
  dlocal Context_err_local, Class_err_local;
  lvars CV;
  lvars List_CV = back(Expr);             ;;; get list of condition-value pairs
  lvars lab_next = sysNEW_LABEL();       ;;; where to go for the next CV pair
  lvars lab_done = sysNEW_LABEL();       ;;; where to go when evaluation is done
  lvars else_?   = false;
  for CV in list_from_scm(List_CV) do   ;;;
     CV -> Context_err_local;
     '(<test> <value>) clause'
                -> Class_err_local;
     if atom(CV) then                   ;;; Check correct form.
          mishap_scm(Msg_wrong_cond,
                        [^CV ^Expr]);
     endif;
    lvars C = front(CV);                   ;;; Get the condition.
    sysLABEL(lab_next);                 ;;; come here to try new condition.
    sysNEW_LABEL() -> lab_next;          ;;; and go here if you fail.
    if C == else_scm then                  ;;; else? do it unconditionally.
      compile_expr_seq(back(CV),Env);
      true -> else_?;
    else                                ;;; otherwise we must do a test.
      compile_bool(C,Env);
      sysIFNOT(lab_next);               ;;; test fails? try next pair
      compile_expr_seq(back(CV),Env);         ;;; succeeds? get the value
      sysGOTO(lab_done);                ;;; and go to the end
    endif;
  endfor;
  unless else_? then
    sysLABEL(lab_next);
    sysPUSHQ('Undefined from cond');
  endunless;
  sysLABEL(lab_done);                   ;;; whole expression complete.
enddefine;

compile_cond -> method("cond");

/*
else is a reserved word.
-------------------------
*/
lconstant msg_else =
   '"else" apparently used as a function in %p. Badly formed cond or case?';

define compile_else(Expr,Env);
    mishap_scm(msg_else,[^Expr])
enddefine;


compile_else -> method("else");

/*
  compile_begin handles the (begin E1...En) construct
-----------------------------------------------------
*/

define compile_begin(Expr,Env);
  dlocal Context_err = Expr;
  compile_expr_seq(back(Expr),Env)
enddefine;

compile_begin -> method("begin");

/*
  compile_do handles the do expression
--------------------------------------
*/
define compile_do(Expr,Env);
    unless length_scm(Expr) >= 3 then
        mishap_scm('Do expression "%p" missing a part', [^Expr]);
    endunless;

    let Bindings = front(back(Expr)),
        Exit     = front(back(back(Expr))),
        Body     = back(back(back(Expr))),
        loop     = sysNEW_LABEL(),
        out      = sysNEW_LABEL(),
    in
        push_Bindings(Bindings,Env,true);          ;;; we evaluate ALL expressions
        sysLBLOCK(false);
        make_Bindings(Bindings,Env);             ;;; before binding the variables.
        compile_debug("do",maplist(Bindings,front),Env);

        sysLABEL(loop);                        ;;; begin iteration
        compile_expr(front(Exit), Env);        ;;; make the test
        sysIFSO(out);
        compile_expr_seq_0(Body,Env);           ;;; obey the commands
        lvars B;
        for B in Bindings do
           if length(B) == 2 then               ;;; no step
           elseif length(B) == 3 then
             compile_expr(front(back(back(B))),Env);
           else mishap_scm(
            'Badly formed binding "%p%" in do expr "%p"',
             [^Bindings ^Expr])
           endif;
        endfor;

        for B in rev(Bindings) do
           if length(B) == 2 then               ;;; no step
           elseif length(B) == 3 then
             sysPOP(front(B))
           endif;
        endfor;

        sysGOTO(loop);
        sysLABEL(out);
        compile_expr_seq_0(back(Exit),Env);
        sysENDLBLOCK();
    endlet
enddefine;

compile_do -> method("do");

/*
  compile_let handles let and let* and letrec expressions.
-------------------------------------------------------------
??? we have not yet got this right, since we need to have the correct
lexical blocks.
*/


define compile_let(Expr,Env);
    let was_executing = popexecute in
        unless length_scm(Expr)>2 then
            mishap_scm('Missing binding or body in let expr\n %p',
                [^Expr]);
        endunless;
        dlocal Context_err = Expr;
        lvars Bindings = front(back(Expr)),
            Body     = back(back(Expr));
        ;;;  sysLBLOCK(false);                        ;;; let introduces new lexical nest.
        lvars f = front(Expr);
        if f==let_scm then                         ;;; For let..
            push_Bindings(Bindings,Env,false);             ;;; we evaluate ALL expressions
            sysLBLOCK(false);
            make_Bindings(Bindings,Env);             ;;; before binding the variables.
            compile_debug_1(f,maplist(Bindings,front),Env,
                            was_executing);
            compile_expr_seq(Body,Env);              ;;; and then call the body.
            sysENDLBLOCK();
        elseif f==letrec_scm then                  ;;; letrec
            sysLBLOCK(false);
            declare_Bindings(Bindings);
            compile_debug_1(f,maplist(Bindings,front),Env,
                             was_executing);
            push_Bindings(Bindings,Env,false);             ;;; we evaluate ALL expressions
            make_Bindings(Bindings,Env);             ;;; before binding the variables.
            compile_expr_seq(Body,Env);              ;;; and then call the body.
            sysENDLBLOCK();

        else                                     ;;; whereas for let* ...
            sysLBLOCK(false);
            do_Bindings(Bindings,Env);
            compile_debug_1(f,maplist(Bindings,front),Env,
                              was_executing);
            compile_expr_seq(Body,Env);
            sysENDLBLOCK();
        endif;
    endlet
enddefine;

compile_let -> method("let");
compile_let -> method("letrec");
compile_let -> method('let*');

lvars
  Msg_bad_binding =
    'Badly formed binding %p in bindings %p',
  Msg_need_var_bndg =
   'Variable needed instead of "%p" in binding "%b" in bindings "%p"';

define declare_Bindings(Bindings);
    lvars Binding;
    for Binding in Bindings do
        let V = front(Binding) in
            LVARS_scm(V);
        endlet
    endfor;
enddefine;


/*
   check_Binding checks the syntactic form of a binding  (v,E)
-------------------------------------------------------------
*/

define check_Binding(Binding,Bindings);
    if atom(Binding) then
       mishap_scm(Msg_bad_binding,
                     [%Binding,Bindings%])
    elseunless is_var(front(Binding)) then
       mishap_scm(Msg_need_var_bndg,
            [%front(Binding), Binding,Bindings%])
    endif
enddefine;

;;; push_Bindings generates code to push the RHS values of a sequence
;;; of bindings on the stack.

define push_Bindings(Bindings,Env,do_?);
  lvars Binding;
  for Binding in list_from_scm(Bindings) do
    unless length(Binding) == 2 or do_? then
      mishap_scm('Binding "%p" is not of form (<var> <expr>)',[^Binding]);
    endunless;
    check_Binding(Binding,Bindings);
    compile_expr(front(back(Binding)),Env);
  endfor;
enddefine;

;;; make_Bindings generates code to pop the stacked-up bindings into
;;; variables.

define make_Bindings(Bindings,Env);
    unless ispair(Bindings) then
      mishap_scm('Bindings %p should be a list', [^Bindings]);
    endunless;
    let Vars = maplist(Bindings,front),
        Binding
    in
        check_duplicate(Vars,"let",Env);
        for Binding in rev_scm(Bindings) do
            let V = front(Binding) in
                LVARS_scm(V);
                sysPOP(V);
            endlet
        endfor;
    endlet
enddefine;

;;; do_Bindings generates code to do bindings se

define do_Bindings(Bindings,Env);
    lvars Binding;
    for Binding in list_from_scm(Bindings) do
        let V = front(Binding) in
            check_Binding(Binding,Bindings);
            compile_expr_seq(back(Binding),Env);
            LVARS_scm(V);
            sysPOP(V);
        endlet
    endfor;
enddefine;


/*
  compile_set_! treats the assignment statement in Scheme.
-----------------------------------------------------------
The form is (set! Var Expr). We have to check the form for length (1) and
for Var being a variable (2). If all is OK (3), we generate code to evaluate
the Expr and assign it to the Var.
*/

define compile_set_!(E,Env);
  dlocal Context_err_local = E;
  unless length_scm(E) == 3 then                         ;;; (1) right form?
    mishap_scm('Wrong form (  %p  ) for "set!" statement',
            [^E]);
  endunless;
  lvars (_,Var,Val) = explode(E);
  unless is_var(Var) then                                ;;; (2)
    mishap_scm(
         'Non-atom %p cannot be bound by "set!" in %p',
         [%Var,E%]);
  endunless;
  compile_expr(front(back(back(E))),Env);                ;;; (3)
  sysPUSHS(undef);                           ;;; duplicate top of stack.
  sysPOP(Var);
enddefine;

compile_set_! -> method('set!');

/*
  compile_trace supports the tracing of function calls
-------------------------------------------------------
*/

define compile_command(E,Env,Cmd);
  lvars Names = back(E);
  popval([^Cmd ^^Names ;]);
  sysPUSHQ(undef_scm);
enddefine;

compile_command(%"trace"%) -> method("trace");
compile_command(%"untrace"%) -> method("untrace");
compile_command(%"untraceall"%) -> method("untraceall");

;;; lconstant systrace_proc_old = systrace_proc;



/*
Setting up the standard bindings for built-in functions.
-------------------------------------------------------
*/

/*
  display
-------------------
*/

define display_(/*..*/,n);
  dlocal pr_quotes_scm = false;
  dlocal cucharout;
  lvars L = listify_count(n);
  lvars Rest = back(L);
  if Rest/==nil_scm then
       rep_char_file(front(Rest)) -> cucharout;
  endif;
  pr_scm(front(L));
  undef_scm;
enddefine;


define write_(/*..*/,n);
  dlocal pr_quotes_scm = true;
  dlocal cucharout;
  lvars L = listify_count(n);
  lvars Rest = back(L);
  if Rest/==nil_scm then
       rep_char_file(front(Rest)) -> cucharout;
  endif;
  pr_scm(front(L));
  undef_scm;
enddefine;


/*
define read_(port);
  port();
enddefine;
*/

define pr_string_scm(S,do_backslash);
    if not(do_backslash) then pr(S)
    else
        let i, n = datalength(S) in
            for i from 1 to n do
                let c = subscrs(i,S) in
                    if      c == `"` or c == `\\` then
                        charout(`\\`); charout(c);
                    elseif  c == `\n` then
                        charout(`\\`); charout(`n`);
                    else charout(c)
                    endif

                endlet
            endfor
        endlet
    endif
enddefine;

/*
  pr_scm is the general procedure to print a scheme object
-------------------------------------------------------------
??? we need to make strings print correctly with \... chars
*/

define  pr_symbol_scm(L);
    if subscrw(datalength(L),L) == char_Symbol_flag_scm then
        let c = subscrw(1,L) in
            if c == `(` or c == `)` or c == `,`  or c == `;`
            or c == `{` or c == `}`
            then
                  pr('##')
            endif;
        endlet;
        pr(allbutlast(1,L))
    else pr(L);
    endif;
enddefine;


define pr_scm_n(L,n_indent);
    ;;;printf('n_indent = %p ', [^n_indent]);
    if isprocedure(L) then
        pr('<Compiled function: ');
        pr_scm(pdprops(L));
    ;;;    if isclosure(L) then pr_scm_n(datalist(L),n_indent)
    ;;;    endif;
        pr(' >');
    elseif isword(L) then pr_symbol_scm(L)
    elseif isundef(L) then
      printf('Uninitialised variable \'%p', [%undefword(L)%]);
    elseif isstring(L) then
        dlocal pop_pr_quotes = false;
        if pr_quotes_scm then pr(""");
        endif;
        pr_string_scm(L,pr_quotes_scm);
        if pr_quotes_scm then pr(""");
        endif;
    elseif L==[] then pr('()')
    elseif L==true then pr('#t')
    elseif L==false then pr('#f')
    elseif is_char(L) then pr('#\\');
        let n = code_char(L) in
            if n==` ` then pr('space')
            elseif n== `\n` then pr('newline');
            elseif n== `\t` then pr('tab');
            else charout(n)
                endif
            endlet
    elseif isratio(L) then
        let (n,d) = destratio(L) in
            pr_scm_n(n,n_indent); pr("/"); pr_scm(d);
        endlet;
    elseif iscomplex(L) then             ;;; complex number
        let (x,y) = destcomplex(L) in
           pr_scm_n(x,n_indent);
              if y>=0 then pr('+'); pr_scm(y); pr('i');
              else pr('-'); pr_scm(-y); pr('i');
              endif;
        endlet
    elseif ispair(L) then pr_list_scm(L,n_indent)
    elseif atom(L) then pr(L)
    endif;
enddefine;

pr_scm_n(%0%) -> pr_scm;
pr_scm -> class_print(nil_key);
pr_scm -> class_print(undef_key);
pr_scm -> class_print(procedure_key);
pr_scm -> pr_GDB;

define big(L,n);
  if atom(L) then false
  elseif n==0 then true
  elseif big(front(L),n-1) then true
  else big(back(L),n-1)
  endif;
enddefine;



define pr_list_scm(L,n_indent);
   ;;; printf('n_indent in pr_list = %p  ', [^n_indent]);
    n_indent + 2 -> n_indent;
    let l1 = front(L) in
        if l1 = quote_scm and length_scm(L) == 2 then
            pr('\''); pr_scm_n(front(back(L)),n_indent);
        elseif method(l1) and big(L,10) then
            nl(1); sp(n_indent); pr("(");
;;;            n_indent + 2 -> n_indent;
            while not(atom(L)) do
                pr_scm_n(front(L),n_indent);
                if l1 == define_scm or l1 == lambda_scm then
                    false -> l1
                else
                    nl(1); sp(n_indent);
                endif;
                back(L) -> L;
                unless atom(L) then pr(' ')
                endunless;
            endwhile;
            unless L==nil_scm then pr(' . '); pr_scm_n(L,n_indent)
            endunless;
            pr(")");

        else pr("(");
            while not(atom(L)) do
                pr_scm_n(front(L),n_indent);
                back(L) -> L;
                unless atom(L) then pr(' ')
                endunless;
            endwhile;
            unless L==nil_scm then pr(' . '); pr_scm_n(L,0)
            endunless;
            pr(")");
        endif
    endlet
enddefine;

lconstant name_Scheme = newassoc([]);

define lconstant pr_scm_proc(p);
    let Description = pdprops(p) in
        if is_symbol_scm(Description) then
            sp(4); npr(Description); true -> shown_GDB;
        elseif isword(Description) and issubstring('<lambda',Description)
            then sp(4); npr(Description); true -> shown_GDB;
        elseif isstring(Description) then
            sp(4); npr(Description); true -> shown_GDB;
        else
            let name_scm = name_Scheme(p) in
                if name_scm  then
                    sp(4); npr(name_scm);
                    true -> shown_GDB;
                elseif show_all_? then
                    pr(' pop11:'); npr(Description);
                    true -> shown_GDB;
                endif
            endlet
        endif;
    endlet;
enddefine;

define pr_proc_GDB = pr_scm_proc;
enddefine;

define exec_scm(/**/);
  last([% 'Undefined Base of Stack';
  sysEXECUTE()%]);
enddefine;

define not_scm(x,n);
   if n/==1 then mishap_scm('not called with >1 argument',[])
   endif;
   not(x);
enddefine;

define Schemify_bool_fun(f,Name)->f_bool;
    lvars m = pdnargs(f);
    if m==1 then
        procedure(/*..*/,n) ->r with_nargs 2;
            unless n==m then
                mishap_scm(Msg_wrong_args,[%Name,n,m%])
            endunless;
            f(/*..*/)->r;
        endprocedure -> f_bool;
    else
        procedure(/*..*/,n) ->r with_nargs 3;
            unless n==m then
                mishap_scm(Msg_wrong_args,[%Name,n,m%])
            endunless;
            f(/*..*/)->r;
        endprocedure -> f_bool;
    endif;
    unless Name then pdprops(f) -> Name
    endunless;
    'Scheme bool function: ' >< Name >< "(" >< m >< ")"
        -> pdprops(f_bool);
    ;;;m+1 -> pdnargs(f_bool);
   Name -> name_Scheme(f);
enddefine;

/*
  polyadic_from_binary(P,id) makes a scheme polyadic function
--------------------------------------------------------------

Scheme functions like "+" are polyadic. If such a function is commutative
and associative,we can convert the POP-11 procedure -P-  into a Scheme function
as a procedure which calls P n times, where n is the top-of-stack.

We actually need a number of versions of this function.  Addition is
meaningful with 0 arguments, so (+) = 1.
*/


define polyadic_from_binary(P,id);
    let R =
        procedure(/*...*/,n)->val;
            lvars val = id;
            repeat n times P(/*..*/,val) -> val
            endrepeat;
        endprocedure
    in
        Schemify(pdprops(P))
            ->> pdprops(R) -> name_Scheme(P);
        R;
    endlet
enddefine;


define polyadic_from_binary_no_id(P);
  procedure(/*...*/,i_n,n)->val;
    lvars val = i_n;
    repeat n-1 times P(/*..*/,val) -> val
    endrepeat;
  endprocedure;
enddefine;


;;; This is used for non-associative functions like -
;;; (- x y z) = (x-y) - z

define polyadic_from_binary_l_assoc(P,P_unary);
    let P_n =
        procedure(/*...*/,n);
            if n==2 then P(/*..*/)
            elseif n == 1 then P_unary(/*..*/)
            elseif n==0 then
                mishap_scm('function %p needs at least one argument',[%P%])
            else let a_n = /*..*/ in
                   P(P_n(/*...*/,n-1),a_n)
                 endlet
            endif;
        endprocedure
    in pdprops(P) -> pdprops(P_n);
       P_n;
    endlet
enddefine;


define string_to_pop_tokens(s);
  let rep = stringin(s),
     l = pdtolist(incharitem(rep))
  in
    maplist(l,Schemify);
  endlet
enddefine;

/*
string_to_pop_tokens('x := x+2') =>
** (x := x + 2)
*/
/*
  The VAR macro allows us to bind Scheme variables to POP functions.
-------------------------------------------------------------------
*/

define macro VAR;
  lvars V =  mk_atom_scm(itemread());
  ;;;  [VAR %V%] =>
  "vars",V;
enddefine;

define applist_scm(L,P);
  while not(atom(L)) do
    P(front(L));
    back(L) -> L;
  endwhile;
  undef_scm;
enddefine;

define length_scm(list);
  if atom(list) then 0 else 1+length_scm(back(list))
  endif;
enddefine;


define check_null_scm(x);
  unless x == nil_scm then
    mishap_scm('A list is wrongly terminated by %p',[^x]);
  endunless;
enddefine;


define apply_scm(/*...*/ list,n) with_props 'apply ';
  let f = subscr_stack(n-1),
      val = f(/*...*/ explode(list),n+length(list)-2)
  in
     -> ;
     val;
  endlet
enddefine;

example apply_scm
;;; ??? seems to work
;;;  apply_scm(length_scm,[[a b c]],2) =>
endexample

/*
map_scm_list maps f over a list of lists of arguments
*/

define map_scm_list(f,L,n);
    if null(front(L)) then []
    else
        f(applist(L,front),n) :: map_scm_list(f,maplist(L,back),n)
    endif
enddefine;

define map_scm_n(/*...*/,n);
   let L = conslist(/*...*/,n),
       f = /*...*/
   in
       map_scm_list(f,L,n);
   endlet
enddefine;

example map_scm_n
  map_scm_n(procedure(x,y,n); x+y endprocedure, [1 2 3], [4 5 6],2) =>
** [5 7 9]
endexample;

define map_scm_1(list,f);
  if null(list) then []
  else  f(front(list),1) :: map_scm_1(back(list),f)
  endif;
enddefine;

define map_scm(f,list,n);
  if n<2 then
      mishap_scm('map called with %p args, it needs at least 2',
                   [%n%]);
  elseif n==2 then map_scm_1(list,f)
  else map_scm_n(f,list,n-1)
  endif;
enddefine;

example map_scm
  map_scm ( Schemify(conspair), [1 2 3] , [4 5 6], 3) =>
** ((1 . 4) (2 . 5) (3 . 6))
  map_scm ( Schemify(hd) , [[1] [2] [3]], 2) =>
** (1 2 3)
endexample


;;; assoc_scm performs the assoc function ??? do we use == or =???
/*
define assoc_scm(val,list,n);
  unless n==2 then
    mishap_arity("assoc",1,n);
  endunless;
  until atom(list) do
    lvars Pair = front(list);
    if front(Pair) == val then return(Pair)
    endif;
    back(list) -> list;
  enduntil;
enddefine;
*/


define make_string_scm(m,n_args);
    if n_args==1 then
        inits(m)
    elseif n_args == 2 then
        let i, c = code_char(m), m1 = /*...*/, s = inits(m1) in
            for i from 1 to m1 do c -> subscrs(i,s)
            endfor;
            s
        endlet
    else
        mishap_arity('make-string',1,n_args);
    endif
enddefine;

Schemify('make-string') -> pdprops(make_string_scm);


define string_ref_scm(string,k,n_args);
    if n_args==2 then
       cons_char(subscrs(k+1,string))
    else
       mishap_arity('string-ref',2,n_args)
    endif
enddefine;

Schemify('string-ref') -> pdprops(string_ref_scm);


define string_set_scm(string,k,char,n_args);
    if n_args==3 then
       code_char(char) -> subscrs(k+1,string);
       string;
    else
       mishap_arity('string-set!',3,n_args)
    endif;

enddefine;

Schemify('string-set!') -> pdprops(string_set_scm);

define 7 =_ci(s1,s2);
  uppertolower(s1) = uppertolower(s2)
enddefine;

define string_compare_scm(f,op);
  let g =
  procedure(string_1,string_2,n);
      if n==2 then
      let d = f(string_1,string_2) in
        if d==1 then
              op == "<=" or op == ">="
        elseif d then op == "<" or op == "<="
        else op == ">" or op == ">="
        endif
      endlet
      else
       mishap_arity('string-'><op,2,n)
      endif;
  endprocedure
  in
     'string'>< op><'?' -> pdprops(g);
     g;
  endlet
enddefine;

define alphabefore_ci(s1,s2);
  alphabefore(uppertolower(s1),uppertolower(s2))
enddefine;

define get_Env();
  Env_init;
enddefine;

define rev_scm(L)->L1;
  nil_scm->L1;
  while(not(atom(L))) do
    conspair(front(L),L1) -> L1;
    back(L) -> L;
  endwhile;
enddefine;

example rev_scm
rev_scm([1 2 3]) =>
** [3 2 1]
endexample;

lconstant Msg_funny_stack =
   'WARNING: POPLOG stack has changed in size by %p evaluating\n   %p\n'    <>
   'This is a Scheme system error and may result in incorrect execution' <>
   'of your program';

define eval_scm(Expr,Env);
    let s1 = stacklength() in
        compile_expr(Expr,Env);
        exec_scm();
        let s2  = stacklength() in
            unless s1 == s2-1 then
                printf(Msg_funny_stack,
                    [%s2 - s1, Expr%]);
            endunless;
        endlet
    endlet
enddefine;

define newline_scm(n) with_props newline;
  dlocal cucharout;
  if n==1 then
      let file = /* */ in
         rep_char_file(file) -> cucharout;
      endlet
  elseif n>1 then mishap_wrong_args("newline");
  endif;
  pr('\n'); undef_scm;
enddefine;


define alphaless(W1,W2);
  alphabefore(W1,W2) == true
enddefine;

define explode_scm(W);
    let
        i, n=datalength(W)-1
    in
        [%  for i from 1 to n do
                let c = subscrw(i,W) in
                    if isnumbercode(c) then c-`0`
                    else
                        consword(
                            c,char_Symbol_flag_scm,2)
                    endif
                endlet
            endfor
        %]

    endlet
enddefine;

example explode_scm
explode_scm("'fred23 '") =>
** [f  r  e  d  2 3]
endexample


define implode_scm(list);
    consword(applist(list,
            procedure(W);
                subscrs(1,W><'');
            endprocedure), char_Symbol_flag_scm,length(list)+1)

enddefine;

define gensym_scm(W);
  gensym(allbutlast(1,W)) <> ":"
enddefine;

/*
SF(f,name) converts a POP-11 function f to a Scheme function with given name
------------------------------------------------------------------------------
*/

define SF(f,name);
  let f_s = Schemify_function(f,name) in
    Schemify(name) -> pdprops(f_s);
    if isword(pdprops(f)) then
        name -> name_Scheme(f)
    endif;
    f_s
  endlet
enddefine;


define Schemify_char_comp(f,Name)->g;
   procedure(c1,c2,n);
      unless n==2 then
        mishap_scm('Wrong number of arguments for %p',[%Name%]);
      endunless;
      f(code_char(c1),code_char(c2))
   endprocedure -> g;
   Name -> pdprops(g);
   Name -> name_Scheme(f);
enddefine;

;;; Perform case-insensitive comparison.

define Schemify_char_ci_comp(f,Name)->g;
   procedure(c1,c2,n);
      unless n==2 then
        mishap_scm('Wrong number of arguments for %p',[%Name%]);
      endunless;
      f(uppertolower(code_char(c1)),uppertolower(code_char(c2)))
   endprocedure -> g;
   Name -> pdprops(g);
   Name -> name_Scheme(f);
enddefine;

define Schemify_char_prop(f,Name)->g;
   procedure(c1,n);
      unless n==1 then
        mishap_scm('Wrong number of arguments for %p',[%Name%]);
      endunless;
      f(code_char(c1))
   endprocedure -> g;
   Name -> pdprops(g);
   Name -> name_Scheme(f);
enddefine;

define iswhitecode(char);
   char == ` ` or char == `\t`
   or char == `\n` or char == `\r` or char == `\^L`
enddefine;

define reciprocal(x); 1/x
enddefine;

define eqv(x,y);
  if atom(x) and atom(y) then x=y else x==y
  endif;
enddefine;


define open_input_file(Name);
   let rep_char = discin(Name),
       rep_token = tokenise(rep_char)
   in
      cons_file(Name,rep_char,rep_token);

   endlet;

enddefine;


define open_output_file(Name);
   let sink_char = discout(Name)
   in
      cons_file(Name,sink_char,false);
   endlet;
enddefine;

vars channel_tty_scm = tokenise(charin);
vars current_input_port = cons_file('current-input-port',
                                     charin, channel_tty_scm);

vars current_output_port = cons_file('current-output-port',
                                      charout, false);

define read_scm(n) with_props 'read ';
  if n==0 then read_scm(channel_tty_scm,1)
  elseif n==1 then read_sexpr(rep_token_file(/*...*/))
  else mishap_scm('read takes 0 or 1 argument, was given %p',[%n%]);
  endif;
enddefine;


define read_char_scm(n) with_props 'read-char ';
    if n==0 then read_char_scm(channel_tty_scm,1)
    elseif n==1 then
        let
            port = (/**/),
            rep = rep_char_file(port)
        in
            if rep then
                let c = rep() in
                    if c==termin then
                        false -> rep_char_file(port); c
                    else cons_char(c)
                    endif
                endlet
            else termin
            endif
        endlet
    else mishap_scm('read-char takes 0 or 1 argument, was given %p',[%n%]);
    endif;
enddefine;

define symbol_to_string(W);
  let  /*..*/ n = destword(W) in
      -> ;
      consstring(/*..*/, n-1)
  endlet

enddefine;

define string_to_symbol(S);
  let /*...*/ n =    deststring(S)
  in
    consword(/*...*/,char_Symbol_flag_scm,n+1)
  endlet
enddefine;


define lconstant append_for_scm(X,Y);
  if ispair(X) then
    conspair(front(X), append_for_scm(back(X),Y))
  else Y
  endif
enddefine;

lconstant append_scm = polyadic_from_binary_no_id(append_for_scm);
"append" -> pdprops(append_scm);


lconstant max_scm     = polyadic_from_binary_no_id(max);
lconstant min_scm     = polyadic_from_binary_no_id(min);
"max" -> pdprops(max_scm);
"min" -> pdprops(min_scm);

define cd_scm(n) with_props 'cd';
    let Dir = if n==0 then '~' else /*..*/ endif
    in
        Dir -> current_directory;
        undef_scm;
    endlet
enddefine;

define pwd_scm();
  npr(current_directory);
  undef_scm;
enddefine;

define sqrt_search(n,l,u);
    let m = (l + u) div 2,
        v = m*m
    in
        if v = n then m
        elseif l=u or l=u-1 then sqrt(n)
        elseif v<n then sqrt_search(n,m,u)
        else sqrt_search(n,l,m)
        endif
    endlet
enddefine;



define sqrt_scm(z);
    if isintegral(z) then
        sqrt_search(z,1,z div 2 + 1)
    elseif iscomplex(z) then
        let (x,y) = destcomplex(z) in
            if isintegral(x) and isintegral(y) then
               if y == 0 then sqrt_scm(x)
               elseif x == 0 then 0 +: sqrt_scm(y)
               else sqrt(z)
               endif
            else sqrt(z)
            endif
        endlet;
    else sqrt(z)
    endif
enddefine;

example sqrt_scm
  maplist([1 2 3 4 5 6 7 8 9], sqrt_scm) =>
** [1 1.41421 1.73205 2 2.23607 2.44949 2.64575 2.82843 3]
vars n = sqrt_scm(10**100);
n*n = 10**100 =>
** <true>
sqrt_scm(4+:0) =>
** 2
sqrt_scm(0+:4) =>
** 0_+:2
sqrt_scm(3+:4) =>
** 2.0_+:1.0
endexample

define debug_scm(flag);
    if flag==true or flag==false then
        flag -> do_break_?;
        npr(if flag then 'Will' else 'Will not' endif<>
            ' stop at break-points');
    else
        let Msg1 =
            'Code to allow breaks in current function will'
        in
            if flag = Schemify("code") then true -> debugging_?;
                npr(Msg1 <> ' be generated');
            elseif flag = Schemify("'no-code'") then false -> debugging_?;
                npr(Msg1 <> ' NOT be generated');
            else npr('Illegal argument for debug: ')<>flag;
            endif;
        endlet
    endif;
    undef_scm;
enddefine;

VAR 'atom?'  = Schemify_bool_fun(atom,'atom?');
VAR 'apply'  = apply_scm;
;;;VAR assoc  = assoc_scm;
VAR 'null?'  = Schemify_bool_fun(nonop == (% [] %),'null?');
VAR 'alphaless?' = Schemify_bool_fun(alphaless,"'alphaless?'");
VAR append   = append_scm;
VAR +      = polyadic_from_binary(nonop +,0);
VAR *      = polyadic_from_binary(nonop *,1);
VAR -      = polyadic_from_binary_l_assoc(nonop -,negate);
VAR /      = polyadic_from_binary_l_assoc(nonop /, reciprocal);
VAR =      = Schemify_bool_fun(nonop =,false);
VAR >      = Schemify_bool_fun(nonop >,false);
VAR <      = Schemify_bool_fun(nonop <,false);
VAR <=      = Schemify_bool_fun(nonop <=,false);
VAR >=      = Schemify_bool_fun(nonop >=,false);

VAR ascii  = SF(procedure(W); subscrw(1,W) endprocedure,'ascii');
VAR car    = SF(front,'car');
;;;VAR caar    = SF(front<>front,'caar');
VAR cadr    = SF(back<>front,'cadr');
VAR cdr     = SF(back,'cdr');
;;;VAR cadar   = SF(front<>back<>front,'cadar');
;;;VAR caddr   = SF(back<>back<>front,'caddr');
;;;VAR cadddr   = SF(back<>back<>back<>front,'caddr');
VAR cdar    = SF(front<>back,'cdar');
VAR cd      = cd_scm;
VAR 'char?' = Schemify_bool_fun(is_char,'char?');
VAR 'char-alphabetic?' = Schemify_char_prop(isalphacode,'char-alphabetic?');
VAR 'char-numeric?' = Schemify_char_prop(isnumbercode,'char-numeric?');
VAR 'char-whitespace?' = Schemify_char_prop(iswhitecode,'char-whitespace?');
VAR 'char-upper-case?' = Schemify_char_prop(isuppercode,'char-upper-case?');
VAR 'char-lower-case?' = Schemify_char_prop(islowercode,'char-lower-case?');
VAR 'char-upcase' = Schemify_char_prop(lowertoupper<>cons_char,'char-upcase');
VAR 'char-downcase' = Schemify_char_prop(uppertolower<>cons_char,'char-downcase');

VAR 'char=?'= Schemify_bool_fun(nonop =,'char=?');
VAR 'char<?'= Schemify_char_comp(nonop <,'char<?');
VAR 'char>?'= Schemify_char_comp(nonop >,'char>?');
VAR 'char<=?'= Schemify_char_comp(nonop <=,'char<=?');
VAR 'char>=?'= Schemify_char_comp(nonop >=,'char>=?');

VAR 'char-ci=?'= Schemify_char_ci_comp(nonop =,'char-ci=?');
VAR 'char-ci<?'= Schemify_char_ci_comp(nonop <,'char-ci<?');
VAR 'char-ci>?'= Schemify_char_ci_comp(nonop >,'char-ci>?');
VAR 'char-ci<=?'= Schemify_char_ci_comp(nonop <=,'char-ci<=?');
VAR 'char-ci>=?'= Schemify_char_ci_comp(nonop >=,'char-ci>=?');

VAR 'char->integer' = SF(code_char,'char->integer');
VAR char   = SF(procedure(n); consword(n,char_Symbol_flag_scm,2)
                endprocedure,'char');


VAR 'close-input-port'
           = SF(procedure(f);
                   sysclose(discin_device(rep_char_file(f)));
                   undef_scm;
                endprocedure,
              'close-output-port');

VAR 'close-output-port'
           = SF(procedure(f);
                   rep_char_file(f)(termin);
                   undef_scm;
                endprocedure,
              'close-output-port');
VAR cons   = SF(conspair,'cons');

VAR 'current-input-port' = SF(procedure(); current_input_port
                               endprocedure, 'current-input-port');

VAR 'current-output-port' = SF(procedure(); current_output_port
                               endprocedure, 'current-output-port');
VAR debug = SF(debug_scm,"debug");
VAR display = display_;
VAR exit    = SF(sysexit,"exit");
VAR expt    = SF(nonop **,"expt");
VAR write   = write_;
VAR 'eof-object?' = Schemify_bool_fun(nonop ==(%termin%),'eof-object?');
VAR 'eq?'     = Schemify_bool_fun(nonop ==, 'eq?');
VAR 'eqv?'    = Schemify_bool_fun(eqv, 'eqv?');
VAR 'equal?' = Schemify_bool_fun(nonop =, 'equal?');
VAR eval   = SF(eval_scm,'eval');
VAR explode = SF(explode_scm,'explode');
VAR false   = false_scm;
VAR 'generate-uninterned-symbol' = SF(gensym_scm,
                                       'generate-uninterned-symbol');
VAR implode = SF(implode_scm,'implode');
VAR 'input-port?' = SF(is_file,'input-port?');
VAR 'integer?' = Schemify_bool_fun(isintegral,'integer?');
VAR 'integer->char' = SF(cons_char,'integer->char');
VAR last    = SF(last,'last');
;;;VAR list    = exec_scm(compile_expr( Schemify([lambda x x]), Env_init));
VAR 'list->string'
            = SF(procedure(l); consstring(dl(),length(l)) endprocedure,
                        'list->string');
VAR load    = SF(schemecompile<>identfn(%true%),'load');
VAR length  = SF(length_scm,'length');
;;;VAR 'list?' = Schemify_bool_fun(islist,'list?');
VAR 'make-string' = make_string_scm;
VAR map     =    map_scm;
VAR max     = max_scm;
VAR min     = min_scm;
;;;VAR 'member'   = Schemify_bool_fun(member,'member');
VAR 'nearest-repl/environment' = SF(get_Env,'nearest-repl/environment');
VAR newline = newline_scm;
VAR not     = not_scm;
VAR 'number?' = Schemify_bool_fun(isnumber,'number?');
VAR 'open-input-file' = SF(open_input_file,'open-input-file');
VAR 'open-output-file' = SF(open_output_file,'open-output-file');
VAR 'output-port?' = SF(is_file,'output-port?');
VAR 'pair?'   = Schemify_bool_fun(ispair,'pair?');
VAR 'procedure?' = Schemify_bool_fun(isprocedure,'procedure?');
VAR pwd       = SF(pwd_scm,"pwd");
VAR read      = read_scm;
VAR 'read-char'  = read_char_scm;
VAR remainder = SF(nonop rem,'remainder');
VAR reverse   = SF(rev,'reverse');
VAR quotient  = SF(nonop div,'quotient');
VAR 'set-car!' = SF(procedure(pair,obj); obj->front(pair);obj endprocedure,
                    'set-car');
VAR 'set-cdr!' = SF(procedure(pair,obj); obj->back(pair);obj endprocedure,
                    'set-cdr');

VAR sqrt = SF(sqrt_scm,"sqrt");
VAR 'string=?'= Schemify_bool_fun(nonop =,'string=?');
VAR 'string-ci=?' = Schemify_bool_fun(nonop =_ci,'string-ci=?');

VAR 'string<?'= string_compare_scm(alphabefore,"<");
VAR 'string>?'= string_compare_scm(alphabefore,">");
VAR 'string<=?'= string_compare_scm(alphabefore,"<=");
VAR 'string>=?'= string_compare_scm(alphabefore,">=");


VAR 'string-ci<?'= string_compare_scm(alphabefore_ci,"<");
VAR 'string-ci>?'= string_compare_scm(alphabefore_ci,">");
VAR 'string-ci<=?'= string_compare_scm(alphabefore_ci,"<=");
VAR 'string-ci>=?'= string_compare_scm(alphabefore_ci,">=");

VAR 'string-append' =  polyadic_from_binary_no_id(nonop <>);
VAR 'string-ref' = string_ref_scm;
VAR 'string-set' = string_set_scm;
VAR 'string?' = Schemify_bool_fun(isstring,'string?');
VAR 'string->list' = SF(datalist,'string->list');
VAR 'string->token-list-pop11' = SF(string_to_pop_tokens,
        'string->token-list-pop11');
VAR 'string->symbol' = SF(string_to_symbol,'string->symbol');
VAR 'string-length' = SF(datalength,'string-length');
VAR 'string-ref' = string_ref_scm;
VAR 'symbol?'  = Schemify_bool_fun(isword,'symbol?');  ;;; allows for gensym
VAR 'symbol->string' = SF(symbol_to_string,'symbol->string');
VAR tabto     = SF(tabto<>identfn(%undef_scm%), 'tabto');
VAR true      = true_scm;
VAR 'zero?'     = Schemify_bool_fun(nonop == (%0%),'zero?');


/*
Errata.
---------

  Missing or erroneous scheme functions.
----------------------------------------



check case statement - does it need an equal test or an eq test?
The = == correspondence to eq,=,equal needs to be sorted.

call/cc

check all lists end with nil in member applist etc.

display can take a stream as argument.

for-each should be polyadic.

nearest-repl/environment - there is a hack in here


reset

sqrt_scm(3+:4) gives 2.0 +:1.0, not exact (MIT Scheme has same property).

vectors


  Pop-11 compatibility
---------------------
- clean up. Use pop11 nil for end of list .


  Lexical analysis
-------------------
upper-to-lower

strings need escape handling.
numbers are POP-11 form
boolean constant   #t (should this be <true> ?)


    Special forms not done
---------------------------
begin
do
error
fluid-let   (the dlocal construct)
sequence
cons-stream
the-environment
delay
make-environment
*/


/*
The following code is adapted from Poplog Common Lisp.
*/

define Add_scheme_extn(file);
  file>< '.scm';
enddefine;

define lvars Scheme_setup();
;;;    unless Scheme_setup_done do
;;;        reset_streams();
;;;        true -> Scheme_setup_done
;;;    endunless
enddefine;


define lconstant Scheme_vedsetup();
    unless vedsetupdone do
        Add_scheme_extn('temp') -> vedvedname;
        'scheme' ->> vedhelpname -> vedteachname;
        'index' -> vedrefname;
        Add_scheme_extn('output')
            ->> vedlmr_errs_in_file -> vedlmr_print_in_file
    endunless
enddefine;

lconstant date_built = sysdaytime();

define lconstant Scheme_banner();
    nprintf('%S (Version %P - built %p)',
              [%'UMASS Scheme',  '1.4', date_built%]);
    npr('New extended set! facility');
    unless popunderx then
        npr('Debug facility for remote users');
        npr('Within VED, debugger goes to source-line in code');
        npr('(debug #t)  makes program stop at breakpoints');
        npr('(debug #f)  makes program not stop at breakpoints');
        npr('(trace <function>) prints arguments on entry, result on exit');
        npr('(untrace <function>) stops trace, (untraceall) stops all tracing');
        npr('Hit ? on BREAK for options');
    endunless
enddefine;

define lconstant tryschemecompile(file);
    lvars dev file;
    if (readable(file) ->> dev) then
        printf('Compiling scheme file %p\n', [^file]);
        subsystem_compile(dev, "scheme");
        true
    else
        false
    endif
enddefine;


define lconstant Scheme_xsetup = xsetup_GDB;
enddefine;

define lconstant Scheme_initcomp();
    true -> debugging_?;
    false -> do_break_?;
    if systranslate('DISPLAY') then Scheme_xsetup();
    else 1 -> n_VarFrames;
    endif;
    dialogue_lock(false);
    ;;;systrace_proc_scm -> systrace_proc;
    tryschemecompile(Add_scheme_extn('$popsys/init')) ->;
    unless tryschemecompile(Add_scheme_extn('$poplib/init')) do
        tryschemecompile(Add_scheme_extn('init')) ->
    endunless;
enddefine;




define lconstant Scheme_poparg1();
    dlocal interrupt = sysexit;

    define lconstant Expr_comp() with_nargs 1;
;;;        dlocal load_print = true;
        schemecompile()
    enddefine;

    sys_process_poparg1(Expr_comp, tryschemecompile, Add_scheme_extn(''))
enddefine;


define lconstant Scheme_reset();
;;;    reset_streams();
;;;    nil ->> applyhook ->> evalhook -> read_suppress;
;;;    10 -> read_base;
    if systrmdev(pop_charin_device) then
        charout('\nSetscheme\n')
    endif
enddefine;





' Procedures for switching into / running Poplog  Scheme ' =>



define  setpop11();
    "pop11" -> subsystem;
    ':' -> popprompt;
    setpop()
enddefine;

define  setscheme();
    pr('setting subsystem to scheme in setscheme\n');
    "scheme" -> subsystem;
    pr('setting prompt to => \n');
    '=>' -> popprompt;
    pr('calling setpop()\n');
    mishap_scm -> prmishap;
    setpop()
enddefine;

VAR reset     = SF(setscheme,'reset');

define  macro Scheme;
    "scheme" -> sys_compiler_subsystem(`c`);
     setscheme();
enddefine;

define scheme_compile(file);
    schemecompile(file);
enddefine;

schemecompile('$popscheme/initial.scm');
;;;setpop();

lconstant scheme_subsystem_procedures =
    {^scheme_compile        ;;; SS_COMPILER
     ^Scheme_reset            ;;; SS_RESET
     ^Scheme_setup            ;;; SS_SETUP
     ^Scheme_banner           ;;; SS_BANNER
     ^Scheme_initcomp         ;;; SS_INITCOMP
     ^Scheme_poparg1          ;;; SS_POPARG1
     ^Scheme_vedsetup         ;;; SS_VEDSETUP
     ^Scheme_xsetup           ;;; SS_XSETUP
    };

;;;[scheme ^scheme_subsystem_procedures] :: sys_subsystem_table
;;;                           -> sys_subsystem_table;

;;; Make a scheme subsystem.

'making subsystem' =>

subsystem_add_new("scheme", scheme_subsystem_procedures,
                   '.scm', '=>',  ;;; extension, prompt
                   [],'Poplog Scheme');




/*
From lispved.p of the Common Lisp source.

--- Copyright University of Sussex 1993. All rights reserved. ----------

 */

vars filetypes_scm = ['.scm' '.scheme'];
vars Name_subsystem_scheme = "scheme";

applist(filetypes_scm,
  procedure(ft);

    [;;;[^ft {popcompiler ^scheme_compile}]
     [^ft {subsystem Name_subsystem_scheme}]
     [^ft {vedcompileable true}]
     [^ft {vedprmishap mishap_scm}]
    ]
    <> vedfiletypes -> vedfiletypes;
  endprocedure);

/* Set up wiggly brackets */
'setting up wiggly brackets ' =>

define :ved_runtime_action;
    lvars type;
    dlocal vedargument, vedcurrent, vedmessage;
    for type in filetypes_scm do
        'temp' <> type -> vedcurrent;
;;;        '' -> vedargument; ved_wiggle();
        '()' -> vedargument;
        ved_wiggle();
    endfor;
enddefine;

'setting up runtime action' =>

define :ved_runtime_action;
    vedsetkey('\^[)', vedclosebrackets(% `)` %));
enddefine;


/* Scheme text item boundaries */

'setting up character types for ved' =>

lvars procedure Saved_vedchartype;

/*
Define vedchartype for Scheme, using chartype
We treat numeric and alphabetic characters alike for scheme, since we
can have identifiers like 1+

;;; 0 = whitespace, 1 = identifier, 2 = number, 3 = delimiter, 5=error
*/

define Scheme_vedchartype(char);
    if subsystem == "scheme"
    and caller(1) /== veddocommand then
        let code = chartype(char) in
            if code == 0 then                   ;;; whitespace
                `\s`
            elseif code == 1 then               ;;; identifier
                `\a`
            elseif code == 2 then               ;;; number
                `\a`
            elseif code == 3 then               ;;; delimiter
                 `.`
            elseif char == `'` then
                 `.`
            else char
            endif
        endlet
    else
        Saved_vedchartype(char)
    endif
enddefine;


define :ved_runtime_action;
    2 -> vedindentstep;
    10 -> vedmaxscrollvert;
    dialogue_lock(true);
    vedchartype -> Saved_vedchartype;
    Scheme_vedchartype -> vedchartype;
enddefine;


/* Compilation */

/*
define global ved_==();
    lvars string;
    lconstant Ved_status_output = make_stringout_stream(false);

    define lconstant Ved_status_interrupt();
        get_stringout_string(Ved_status_output) ->;
        'INTERRUPTED' -> vedmessage;
        vedscreenbell();
        veddointerrupt()
    enddefine;

    dlocal
        error_output        =   Err_out,
        load_print          =   true,
        scheme_interrupt      =   Ved_status_interrupt,
        standard_output     =   Ved_status_output,
        ;
    schemecompile(make_stringin_stream(vedargument, false, false));
    get_stringout_string(Ved_status_output) -> string;
    consstring
        (#| `=`, `>`, `\s`,
           appdata(string, procedure();
                               if dup() == `\n` then ->; `\s` endif
                           endprocedure)
        |#) -> string;
    vedrestorescreen();
    vedsetstatus(string, false, true);
    vedsetcursor();
    chainfrom(vedprocesschar, vedprocesschar);
enddefine;
*/


/* The Scheme function ved and edit */

'setting up edit function for Scheme'=>

vars default_pathname = '~/default.scm';
vars FileExt_default_GDB = '.scm';
vars HelpDebug_GDB  = '$popscheme/help_debugger.scm';
vars HelpFile_GDB   = '$popscheme/help_scheme.scm';

define edit_scm(n) with_props 'edit ';
    lvars file,x;
    if n==1 then -> x;
    elseif n==0 then 'unnamed.scm' -> x;
    else  mishap_arity('edit','0 or 1',n);
    endif;

    define lconstant Set_ved_f(sym);   ;;; Set ved to find the function
        lvars sym;

        define lconstant Do_ved_f() with_nargs 1;
            -> vedargument;
            chain(ved_f)
        enddefine;

        vedinput(Do_ved_f(% '-x ' >< sym %))
    enddefine;

    false -> file;
    if isprocedure(x) then
        if isword(pdprops(x)) then
            Set_ved_f(pdprops(x))
        endif;
        source_of_proc(pdprops(x)) -> file;
        if file==undef then false -> file
        endif;
    elseif isstring(x) and datalength(x)/==0 then x->file;

    endif;

    unless file do
        vedvedname -> file;
        if file = nullstring then
            default_pathname ->> vedvedname -> file
        endif
    endunless;
    if vedinvedprocess then
        vedinput(vedselect(% file %))
    else
        apply(vedveddefaults, file, vededitor)
    endif;
    undef_scm;

enddefine;

define edit_GDB(f);
  edit_scm(f,1) -> ;
enddefine;

'UMASS SCHEME CONTROL PANEL' -> EditPanel_GDB;


VAR edit = edit_scm;
VAR pop11 = SF(setpop11,"pop11");
VAR ved  = edit_scm;
VAR 'Motif' = SF(compile(%'$popscheme/Motif.p'%)<>identfn(%undef_scm%),"Motif");



/* Disassemble and <ENTER> DMR */

global vars pop_show_code = false;

/*
define disassemble(sym);
    lvars sym;
    dlocal pop_show_code = false;
    useslib("showcode");
    if sym starts_with @LAMBDA then
        true -> pop_show_code;
        compile_lambda(sym) ->
    else
        symbol_name(sym) -> vedfstring;
        advise('Use ED or <ENTER> F to find the source for ~S,\n;;; then do <ENTER> DMR', [^sym])
    endif
enddefine;


define global ved_dmr();
    dlocal pop_show_code = false, vedlmr_print_in_file;
    useslib("showcode");
    if vedlmr_print_in_file == true then
        systmpfile(false, 'dmr_output_', '') -> vedlmr_print_in_file
    endif;
    true -> pop_show_code;
    if vvedmarkhi == 0 then
        ved_lcp()
    else
        ved_lmr()
    endif
enddefine;
*/

/*
ved menus
---------
*/

vars menubar_scm;

define :ved_runtime_action;
   100 -> vedautowrite;
   popval([uses menubar_scm]);
   menubar_scm -> xved_value("defaultWindow", "menubar")
enddefine;

/*
Marking beginning and end of procedures in VED etc.
----------------------------------------------------
*/


/* --- Copyright University of Sussex 1993. All rights reserved. ----------
From ved_mcp.p in POPLOG Common Lisp.
 */

global vars
    vedautoindent  =  false,
    ;


define lconstant Scheme_mbp();
    lvars startline;
    dlocal vedline vedcolumn vvedlinesize;
    vedline -> startline;
    ;;; search back to "(DEF" (upper or lower case) at beginning of line
    repeat
        vedscreenleft();
        if  vedcurrentchar() == `(`
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `D`)
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `E`)
        and (vedcharright(), lowertoupper(vedcurrentchar()) == `F`) then
            vedmarklo();
            return
        endif;
        if vedline == 1 then
            vederror('No DEF... before line ' sys_>< startline)
        endif;
        vedcharup()
    endrepeat
enddefine;


define lconstant Vedclearhead();
    dlocal vednocharinsert = true;
    unless vedcolumn == 1 do
        vedclearhead()
    endunless
enddefine;


define lconstant Vedindentto(n);
    lvars i line n;
    dlocal vedcolumn, vedchanged = false;
    ;;; assumes 'vedsetlinesize' done
    returnif(vvedlinesize == 0 or vedline > vvedbuffersize);
    fast_subscrv(vedline, vedbuffer) -> line;
    fast_for vedcolumn from 1 to vvedlinesize do
        quitif(fast_subscrs(vedcolumn, line) > 32)
    endfast_for;
    fast_for i from vedcolumn to vvedlinesize do
        quitunless(fast_subscrs(i, line) == `;`);
        if (i - vedcolumn) == 2 then
            Vedclearhead();
            return
        endif
    endfast_for;
    unless n == (vedcolumn - 1) do
        Vedclearhead();
        n // vedindentstep -> n -> i;
        fast_repeat n times vedcharinsert(`\t`) endfast_repeat;
        fast_repeat i times vedcharinsert(`\s`) endfast_repeat;
        vedtrimline();
    endunless
enddefine;


define lconstant Scheme_mep();
    lvars char nesting n q s ss;
    dlocal vedline vedcolumn vvedlinesize;

    0 ->> nesting -> n;
    [] -> ss;
    false ->> q -> s;
    vedmarkhi();
    repeat
        vedrepeater() -> char;
        if char == termin then
            if q then
                'Missing closing ' <> consstring(q, 1)
            else
                'Need closing bracket'

            endif;
            chain(vederror)
        endif;
        if s then
            s + 1 -> s;
            unless vedchartype(char) == `a` do
                if s == 2 then 1 else min(s, vedindentstep) endif -> s;
                conspair(s, ss) -> ss;
                s + n -> n;
                false -> s;
            endunless
        endif;
        if char == `\n` then
            if q == `;` then
                false -> q
            endif;
            if vedautoindent and not(q) then
                Vedindentto(n)
            endif
        elseif q then
            if char == q then
                false -> q
            endif
        elseif char == `(` then
            nesting + 1 -> nesting;
            1 -> s
        elseif char == `)` then
            nesting - 1 -> nesting;
            quitif(nesting == 0);
            n - (fast_destpair(ss) -> ss) -> n
        elseif char == `;` or char == `"` or char == `|` then
            char -> q
        endif
    endrepeat;
    vedmarkhi();
    if vedautoindent then
        if vedchanged then vedchanged + 1 else 1 endif -> vedchanged
    endif
enddefine;


define lconstant Scheme_jcp();
    dlocal vedline vedcolumn vvedlinesize;
    dlocal vedautoindent = true, vvedmarkprops = false;
    vedmarkpush();
    Scheme_mbp();
    vedmarkfind();
    Scheme_mep();
    vedmarkpop()
enddefine;


define lconstant Scheme_tidy();
    dlocal vedline vedcolumn vvedlinesize;
    dlocal vedautoindent = true, vvedmarkprops = false;
    vedmarkpush();
    vedmarkfind();
    ved_mep();          ;;; Why not Scheme_mep? (John Williams, Jan  8 1993)
    vedmarkpop()
enddefine;

define :ved_runtime_action;
    Scheme_mbp -> subsystem_valof("ved_mbp", "scheme");
    Scheme_mep -> subsystem_valof("ved_mep", "scheme");
    Scheme_jcp -> subsystem_valof("ved_jcp", "scheme");
    Scheme_tidy -> subsystem_valof("ved_tidy", "scheme");
enddefine;



define :ved_runtime_action;
    unless popunderx then
       vedsetkey('\^S',syssleep(%100%));
       vedsetkey('\^Q',identfn)
    endunless
enddefine;


/*
The Scheme macro switches from POP-11
-------------------------------------
*/

'defining Scheme macro' =>

define macro Scheme;
  setscheme();
enddefine;
