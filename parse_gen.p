
;;; parse_gen.p                     R.J.Popplestone sep90-sep91

;;; a parser-generator for POPLOG

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

/*

         CONTENTS - (Use <ENTER> g to access required sections)

 --  expand(r) generates code to make a term in the abstract syntax.
 --  Making parse_gen utilities available to the generated parser at run-time
 --  compile_parse_gen compiles a grammar definition
 --  compile_parse_gen_no_obj is the incremental version.
 --  parse_class_defs parses a sequence of definitions of non-terminals
 --  parse_class_def parses the definition of a non-terminal
 --  parse_operator_declaration supports infix operators.
 --  parse_pop_11 supports  POP-11 included with the grammar defs.
 --  parse_NT parses a non terminal symbol
 --  parse_body parses the body of a non-terminal definition.
 --  parse_tail parses the tail of a production, starting with the "->"
 --  parse_seq parses part of a production following a "->"
 --  parse_option parses an option enclosed in [...] brackets
 --  parse_option_seq parses a seqence enclosed in {...} brackets
 --  Var_result(L) is the name of the variable holding the result of a parse
 --  mk_Code_sep makes code to recognise and count separators in a {...} seq.
 --  mk_Code_term makes code to tidy up after a {...} sequence
 --  conjoin_code makes a POP-11 conjunction out of two code sequences.
 --  code_of_comm generats code to create abstact syntax
 --  parse_ved_decl treats the VED declaration specifying file extensions.
 --  parse_output_decl treats an OUTPUT declaration - where is code to go?
 --  Grammar files have the extension '.grm'
 --  mishap_grm treats errors in grammar definitions.
 --  The macro <$ computes list of tokens

*/

compile_mode:pop11 +defpdr +varsch +constr;

uses lexical_args;
uses memo_T_2_n;
uses int_parameters;
uses parse_gen_RT;
uses parse_gen_SS;


section PARSE_GEN =>
                     parse_gen_compile,
                     compile_parse_gen,
                     compile_parse_gen_no_obj,
                     call_parse_gen,
;;;                     parse_gen
                     <$;


uses compile_pr;
;;;lib compile_pr;
/*
This program reads a grammar definition in an extended BNF form, and
generates parsing procedures for it.

A parser-generator for this NT as a (partial)
sub-system of POPLOG i.e. rules in BNF define a parsing function in POP-11.

Thus:

<expr>             ->   <simple_expr> <relational_op>
                          <simple_expr>                '^t2(^t1,^t3)';
                   ->   <simple_expr>                  '^t1'         ;
<simple_expr>      ->   +  <simpler_expr>             '+(^t1)'     ;
                   ->       -  <simpler_expr>             '-(^t1)'     ;
                   ->       <simpler_expr>                '^t1'        ;


creates a POP function expr and a POP function simple_expr which parse
off a list of tokens and return the remaining list and a Prolog term
representing the semantics of the syntactic category. (Crude sort of parsing
to the yaccers of this world).  I have defined the right compiler to associate
with the extension .grm, so you can mark ranges and load them, as well as to
ved_l1 etc.  But it would be nice to get the ved_?? info. modified for .grm
files.

Addendum - the t1, t2,t3.. of the parsing example are POP variables that
are bound in the parsing to the first, second, third non-terminals occurring
in a production.


Extensions - generate printing procedures.
*/

uses rd_plog_trm;
uses ved_source;

vars procedure(
     mk_Code_sep,
     mk_Code_term,
     mk_term     = prolog_maketerm,      ;;; Makes a term
     arity       = prolog_arity,         ;;;
     arg         = prolog_arg,           ;;;
     functor     = prolog_functor,       ;;;
     is_complex  = prolog_complexterm,   ;;;
     is_atom     = isword,               ;;; The POP-11 "word" datatype is
     pr_term     = prolog_write,

     check,
     check_as_class,
     code_of_comm,
     compile_parse_gen,
     compile_parse_gen_from_rep,
     conjoin_code,
     expand,
     expand_args,
     expand_complex,
     expand_fun,
     is_semantic = newassoc([]),
     is_CON      = newassoc([]),
     mishap_grm,
     parse_class_defs,
     parse_class_def,
     parse_NT,
     parse_operator_declaration,
     parse_option
     parse_option_seq
     parse_output_decl,
     parse_pantex_decl,
     parse_pop_11,
     parse_seq,
     parse_tail,
     parse_ved_decl,
     parse_body,
     pr_code,
     record_definition,
     record_HWM,
     require,
     old_mbp,
     old_mep,
     Var_result,
);


vars
    f_APPLY            = false,
    FN_out             = false,
    found_pattern      = false,
    is_extended        = false,
    parse_first_time,
    L_NT,                  ;;; The non-terminal symbols which have been defined
    L_section = false,
    L_semantic = [],
    L_undef = [],
    literal_NT = [% ";", "[", "]", "{","}", "->", "<" %],
    LW_reserved = [-> ; <
                   ^(consword('`'))
                   ^(consword('\''))
                   ],              ;;; The list of reserved words.
    mon_parse_gen = false,         ;;; set true to see procedure generated.
    NT_current,                    ;;; The non-terminal currently being

    NT_tried,                      ;;; The non-terminal we were looking for at error                                                                    ;;; defined.
    NT_tried1,                     ;;; Its caller.
    LNT_memo       = [],           ;;; The list of memoised Non Terminals.
    L_functors     = [],           ;;; The list of functors that can occur.
    L_max,                         ;;; The "high water mark" of parsing.
    Rep_code       = erase,        ;;; The repeater for outputting POP-11.
    tok_wanted,                    ;;; the token needed at failure.
    LVars_t =  [t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12],
    n_NT    = 0,
;




/*
expand(r) generates code to make a term in the abstract syntax.
---------------------------------------------------------------
*/

define expand(r);                                ;;; Make term on stack
  lvars r;
  [%
    if is_complex(r) then
      lvars i,
        f = functor(r),
        n = arity(r);
      if    f = "^" and n=1 then                 ;;;  ^(f(a1..an)) OR ^(x)
        lvars r1 = arg(1,r);                     ;;; so take value...
        if isword(r1) then                       ;;; we had ^(x), now r1 = x
          "(",
          r1,                                    ;;; push its value on stack
          ")"
        elseif is_complex(r1) then               ;;; we had ^(f(x,y))
          expand_complex(r1,false)
        else                                     ;;; Cannot take value of
          mishap('Must have identifier ',        ;;; constant
            [^r])                                ;;;
        endif
      else                                       ;;; ordinary complex term
        expand_complex(r,true);
      endif;

    elseif isword(r) then                        ;;; A word.
      if is_CON(r) then r
      else
        """,r,""",
      endif
    else                                         ;;; Not a complex term
      r
    endif

  %]
enddefine;

vars Msg_no_pop =
'There is no POP-11 procedure with this name - should be in CON defn?';
vars Msg_arity =
'Constructor used with wrong arity';
vars Msg_no_quote =
'Cant take value of constructor function';

define expand_complex(T,is_quoted_functor);
  lvars i,
    n = arity(T),
    f = functor(T);

  if is_CON(f) then                  ;;; we have a constructor function
    if is_quoted_functor then        ;;; which cannot be used with
      mishap(Msg_no_quote,[^f^T]);   ;;; an ^f(...) construction.
    endif;
    lvars f1 = valof(f);             ;;; Get the POP-11 value
    if isprocedure(f1) then          ;;; and check that it has the right arity
      if arity(f1) = n then
        f,"(",expand_args(T),")"
      else
        mishap(Msg_arity,[^f^T])
      endif
    else                             ;;; No procedure with that name.
      mishap(Msg_no_pop,[^f ^T]);
    endif;

  elseif f_APPLY then                ;;; an ML-style general term constructor
    "f_APPLY","," , "(",             ;;; call it with the function and
    expand_fun(f,is_quoted_functor); ;;; a list of arguments.
    "[","%",expand_args(T),"%","]",
    ")";

  elseif is_semantic(f) then

       f, "(",
        for i from 1 to n do
          explode(expand(arg(i,T))), ",",
        endfor,
        ")"
  else                               ;;; The default
    "mk_term","(",
    expand_args(T);
    expand_fun(f,is_quoted_functor),
    ",",
    n,                                   ;;; push the number of args.
    ")"
  endif
enddefine;

define expand_fun(f,is_quoted_functor);
  if is_quoted_functor then
    """, f, """,
  else
    f
  endif;
enddefine;

define expand_args(T);
  lvars i, n=arity(T);
  for i from 1 to n do                 ;;; f(a1...an)
    explode(
      expand(arg(i,T))            ;;; make i'th argument
    ); ",";
  endfor;
enddefine;



example expand
;;; We have to use prolog_readterm, since $$..$$ will substitute for ^b
vars T = prolog_readterm(); f(a,1,^b).
$$x+2$$ =>
expand(T) =>
** [mk_term ( " a " , 1 , ( b ) , " f " , 3 )]
expand($$a+c*d$$)=>
** [mk_term ( " a " , mk_term ( " c " , " d " , " * " , 2 ) , " + " , 2 )]
expand_complex($$a+b$$,true) =>
endexample

/*
Making parse_gen utilities available to the generated parser at run-time
------------------------------------------------------------------------


Next we make a string that will be inserted in the POP-11 object code
to copy some parse_gen variable values into the environment in which the
parser itself will run (the top level unless a SECTION declaration has been
made.
*/

pop_longstrings;
true -> pop_longstrings;
vars Str_prologue =
'
uses parse_gen_RT;        ;;; Load the run-time support for the parser.

;;; Copy the parse_gen standard NT procedures to the current section.
vars procedure(
  identifier                 = PARSE_GEN$-identifier,
  id_any                     = PARSE_GEN$-id_any,
  obj_any                    = PARSE_GEN$-obj_any,
  int                        = PARSE_GEN$-int,
  real                       = PARSE_GEN$-real,
  number                     = PARSE_GEN$-number,
  string                     = PARSE_GEN$-string,
  semicolon                  = PARSE_GEN$-semicolon,
  rightarrow                 = PARSE_GEN$-rightarrow,
  less                       = PARSE_GEN$-less,
  left_single_quote          = PARSE_GEN$-left_single_quote,
  right_single_quote         = PARSE_GEN$-right_single_quote,
  new_line                   = PARSE_GEN$-new_line,
  lambda                     = PARSE_GEN$-lambda,
  null_G                     = PARSE_GEN$-null_G,
  check                      = PARSE_GEN$-check,
  Parse_expr_p               = PARSE_GEN$-Parse_expr_p,

);
';

->pop_longstrings;

/*
compile_parse_gen compiles a grammar definition
------------------------------------------------
This procedure compiles a grammar definition. It takes the same
arguments as the POP-11 standard procedure compile except for a device.
*/

define compile_parse_gen(Source);
  lvars Source, Rep;
  dlocal                             ;;; Initialise ....
    pop_default_type = '.grm',       ;;; The default file extension.
    FN_out      = false,             ;;; The file name for the output file.
    L_NT        = [],                ;;; The list of defined non-terminals
    L_section  = false,              ;;; Is a section specified in the grammar?
    f_APPLY    = false,              ;;; The general expression building fn.
  ;
  false -> is_extended;

  [-> ; <                           ;;; Initialise the list of reserved words
    ^(consword('`'))
    ^(consword('\''))
  ] -> LW_reserved;

  if isprocedure(Source) then        ;;; Get the appropriate repeater for
    Source                           ;;; source.
  elseif isword(Source)
  or     isstring(Source) then
    discin(Source)
  elseif isdevice(Source) then
    mishap('cant compile device',
      [^Source]);
  else
    mishap('Cant compile grammar',
      [^Source]);
  endif -> Rep;

  lvars                              ;;; The code is written to a temporary
    FN_code = systmpfile(false,      ;;; file with this name.
    'parse_gen',
    '.p');
  dlocal
    Rep_code =  discout(FN_code);    ;;; An output repeater for FN_code

  compile_parse_gen_from_rep(Rep);   ;;; Generate the code.
  Rep_code(termin);                  ;;; Close the temporary file.

  lvars
    FN      = if FN_out then FN_out
    else
       lvars FN1 = sys_fname_nam(Source);
       'grm_' <> FN1<>'.p'
    endif,
    Rep_tmp = discin(FN_code),       ;;; Repeater to read from temp. file.
    Rep_obj = discout(FN),           ;;; Repeater to write to object file.
    ;
  dlocal cucharout =  Rep_obj;       ;;; Redirect output to Rep_obj

  nl(2);
  pr(';;; '>< Source ><              ;;;  Print a header of file name and
    '   ' >< sysdaytime());          ;;;  time of creation.
  nl(1);

  if L_section then                  ;;; Output a section header if reqd.
    pr('\n\nsection  ');
    applist(L_section,pr<>sp(%1%));
    nl(1);
  endif;

  pr(Str_prologue);                   ;;; Make run-time utilities available

  compile_pr_list(            ;;; Make list reserved words available
    LW_reserved,                      ;;; to the parser
    cucharout);
  pr('\n -> PARSE_GEN$-LW_reserved;\n');

  pr('\n vars procedure(\n');        ;;; Declare the defined non-terminals
  applist(L_NT,pr <> nl(%1%));       ;;; as POP-11 procedures.
  pr('\n);\n');

  lvars c;
  while (Rep_tmp()->>c) /= termin do  ;;; Copy the temporary file to
    Rep_obj(c);                      ;;; the object file.
  endwhile;

  if L_section then                  ;;; Terminate the section, if any.
    pr('\nendsection\n');
  endif;

  lvars Wd = consword(substring(              ;;; Make a "vars" statement
    1,                   ;;; in the output file
    datalength(FN)-2,    ;;; which will ensure the
    FN));                ;;; file is only loaded once
  pr('\nvars ' >< Wd >< '= true;\n');

  Rep_obj(termin);                   ;;; Close the object file.
enddefine;


/*
compile_parse_gen_no_obj is the incremental version.
-----------------------------------------------------
This is the incremental version of the compiler, which generates no
object code, and doesn't initialise the free variable lists etc.

*/

define compile_parse_gen_no_obj(Rep);
  dlocal Rep_code = erase,
         L_NT = [];
  compile_parse_gen_from_rep(Rep);
enddefine;

define parse_gen_compile(Rep);
  compile_parse_gen_no_obj(Rep);
enddefine;

vars procedure parse_gen = compile_parse_gen;  ;;; For compatibility;


define compile_parse_gen_from_rep(Rep);
  lvars
       L =  Rep.incharitem.pdtolist,
       W,
;
  dlocal L_undef = [],            ;;; Naughty dlocal.. - holds undefined NTs
             ;

  '' -> NT_current;
  parse_class_defs(L);
  unless null(L_undef) then
      pr('\nThe following classes are undefined ');
      npr(L_undef);
    pr('\nThe following can occur as functors');
    npr(
    syssort(L_functors,procedure x,y;
                        alphabefore(x><'',y><'')
                        endprocedure));
  endunless;

  pr('Memoising ');
  for W in LNT_memo do              ;;; memoise or re-memoise these non-
    pr(W); sp(1);
    compile_pr([memo_T_1_n(^W,2) ->  ^W;],Rep_code);
  endfor;
  nl(1);
enddefine;

/*
parse_class_defs parses a sequence of definitions of non-terminals
--------------------------------------------------------------------
*/

define parse_class_defs(L);
  while (parse_class_def(L)->>L) do
  endwhile
enddefine;

/*
parse_class_def parses the definition of a non-terminal
-------------------------------------------------------
This procedure  parses  the  definition  of a  non-terminal  symbol  or  other
statement in parse_gen.
*/

;;; NO ml. vars Msg_one_AP = 'Only one function at a time can be APPLY\'d';

define parse_class_def(L)->L2;
  lvars L,                          ;;; This list should start with a defn.
    L2,
    L_pop;                        ;;; Holds the POP-11 definition of the NT

  if null(L) or hd(L)="end" then    ;;; End of definitions.
    return(false -> L2);
  endif;

  lvars
    f,
    Tok   = L.hd,
    L1    = parse_NT(L),
    NT = identfn();

  if Tok = ":" then                 ;;; Beginning of a POP-11 sequence to
    parse_pop_11(L.tl) -> L2      ;;; be compiled both at grammar
      -> L_pop;                 ;;; generation time and
    compile_pr(L_pop,Rep_code);            ;;; grammar compilation time.
    return
  endif;

  if Tok = "::" then                 ;;; Beginning of a POP-11 sequence to
    parse_pop_11(L.tl) -> L2       ;;; be compiled using compile
      -> L_pop;                  ;;; This is NOT passed through
    compile(L_pop);                ;;;
    return
  endif;

/*  ML stuff - not in current use
  if Tok = "APPLY" then               ;;; The function to apply.
    L.tl -> L;
    dest(L) -> (f,L);
    if f = "::" then                 ;;; Special case - bug in mlinpop.
      conspair -> f_APPLY;
      compile_pr(
        [lvars f_APPLY = conspair;],
        Rep_code)
    else
      ml_valof(f) -> f_APPLY;
      compile_pr(
        [lvars f_APPLY = ml_valof("^f");],
        Rep_code)
    endif;
    if L.hd = ";" then
       return(L.tl->L2)
    else
       mishap(Msg_one_AP,[L]);
    endif
  endif;
*/

  if Tok = "SEMANTIC" then          ;;; List of names of semantic procs.
    L.tl->L;
    until hd(L) = ";" do
      dest(L) -> (f,L);
      true -> is_semantic(f);
    enduntil;
    return(L.tl->L2);
  endif;

  if Tok = "CON" then          ;;; List of names of semantic procs.
    L.tl->L;
    until hd(L) = ";" do
      dest(L) -> (f,L);
      true -> is_CON(f);
      compile_pr([vars ^f = ml_valof("^f");])
    enduntil;
    return(L.tl->L2);
  endif;

  if Tok = "EXTENDED" then
    true -> is_extended;
    return(L.tl->L2);
  endif;

  if Tok = "MONITOR" then
    L.tl.dest -> (Tok,L2);
    if Tok == "ON"  then true
    elseif Tok == "OFF" then false
    else  mishap_grm('MONITOR ON or MONITOR OFF reqd',L);
    endif -> mon_parse_gen;
    printf('Monitoring of parse_gen is %p\n',[%mon_parse_gen%]);
    return;
  endif;

  if Tok = "OPERATOR" then
    parse_operator_declaration(L.tl) -> L2;
    return
  endif;

  if Tok == "SUBSYSTEM" then
    parse_subsystem(L.tl) -> L2 -> ;
    return;
  endif;

  if Tok == "VED" then
    parse_ved_decl(L.tl) -> L2 -> ;
    return;
  endif;

  if Tok == "PANTECHNICON" then
    parse_pantex_decl(L.tl) -> L2 -> ;
    return;
  endif;


  if Tok = "OUTPUT" then
    parse_output_decl(L.tl) -> L2 -> ;
    return;
  endif;


  if Tok = "INCLUDE" then          ;;; ??? does this work ???
    L.tl->L;
    until hd(L) = ";" do
      dest(L) -> L -> f;
      parse_gen(f);
    enduntil;
    return(L.tl->L2);
  endif;

  if Tok = "MEMOISE" then
    L.tl->L;
    [%until hd(L) = ";" do
        dest(L) -> L;

      enduntil%] -> LNT_memo;
    return(L.tl->L2)
  endif;

  unless L1 then                    ;;; Otherwise, we must have a definition...
    mishap_grm('Non-terminal definition must start with a non-terminal name',
      [^Tok]);
  endunless;

  NT -> NT_current;                     ;;; Make this the current non-terminal
  lvars
;;; ??? replace by (body,L2,junk) = ...
    junk  = parse_body(L1,"if",true),   ;;; parse the body of the definition
    L2    = identfn(),
    body  = identfn(),                    ;;; obtaining (body,L2)
    code  = [define ^NT(L) -> (t_out,L1); ;;; Code to recognise non-terminal
    lvars L,L0,L1,t_out=undef,
    t1, t2, t3, t4, t5,
    t6, t7, t8, t9;
    ^^body
    enddefine;];


  pr_code(code);
  compile_pr(code,Rep_code);          ;;; Compile the code for the NT
  record_definition(NT);
enddefine;


define pr_code(code); lvars code;
  dlocal cucharout = charout;
  if mon_parse_gen then
;;;    printf('is_extended=%p\n',[^is_extended]);
    printf('Code generated is:\n%p\n\n', [^code]); nl(1);
  endif;
enddefine;


define record_definition(NT);
  delete(NT,L_undef) -> L_undef;              ;;; NT is now defined...
  NT::L_NT -> L_NT;
enddefine;

/*
parse_operator_declaration supports infix operators.
-----------------------------------------------------
Parse a declaration for a NT class to be treated as infix operators.
*/

define parse_operator_declaration(L) -> L_out;
  lvars (NT,L1)   = parse_NT(L),
        (expr,L2) = parse_NT(L1),
        (op,L3)   = parse_NT(L2),
        (un_op,L4) = parse_NT(L3),
        (precedence,L5) = dest(L4),
        (un_precedence,L6) = dest(L5),
        (is_more_binding,L7) = dest(L6),

  ;
  unless hd(L7) = ";" then
    mishap('Syntax error in OPERATOR declaration',L);
  endunless;

  lvars p_start = if valof(is_more_binding)(0,1) then
                     pop_max_int
                 else -pop_max_int
                 endif;

  lvars code =
    [define ^NT(L); lvars L;
     Parse_expr_p(L,^expr,^op,^un_op,
                  ^precedence,^un_precedence,
                  nonop ^is_more_binding,^p_start,mk_term)
     enddefine;];

    pr_code(code);


  compile_pr(code,Rep_code);
  record_definition(NT);
  tl(L7) -> L_out;
enddefine;

/*
parse_pop_11 supports  POP-11 included with the grammar defs.
-------------------------------------------------------------
Parse a section of POP_11 code terminated by a semicolon,
at bracket nesting depth 0.
*/

define parse_pop_11(L) -> L-> L_pop;
    lvars L, L_pop, Obj,
        n_paren = 0,                      ;;; The depth of parentheses
        n_bra = 0;                        ;;; the depth of square brackets.
    [%  until (member ((dest(L) -> L ->> Obj),[ ; =>]))
        and n_bra = 0
        and n_paren = 0
        do
            if         Obj = "(" then n_paren + 1 -> n_paren
            elseif  Obj = ")" then n_paren - 1 -> n_paren
            elseif  Obj = "[" then n_bra + 1 -> n_bra
            elseif  Obj = "]" then n_bra - 1 -> n_bra
            endif;
        Obj
        enduntil; Obj
    %] -> L_pop;
enddefine;

/*
parse_NT parses a non terminal symbol
------------------------------------

*/


define parse_NT(L) -> L -> W_class;
    lvars L W_class;
    dlocal prwarning = erase;
    check(L,"<") -> L;
    unless L then return
    endunless;
    lvars
         W_class = L.hd,
         p       = W_class.identprops;
    unless isword(W_class) then
        mishap_grm('Non-terminal name must be POP-11 word',[^W_class]);
    endunless;

    if member(W_class,literal_NT) then             ;;; pseudo non-terminal
       list_union([%W_class%],LW_reserved) -> LW_reserved;
       [QUOTE ^W_class] -> W_class; false -> p;
    endif;

    unless p==undef or p==0 or not(p) then
        mishap_grm('Non-terminal name must NOT be POP-11 reserved word',
                    [^W_class]);
    endunless;
    if p and isundef(valof(W_class))
    and not(member(W_class, L_undef))
    then
      W_class::L_undef -> L_undef
    endif;
        require(L.tl,">") -> L;
enddefine;

define require(L,tok) -> L;
  lvars L,tok;
  if    L.hd = tok then L.tl -> L
  else  mishap_grm('Missing token',[% L.hd, tok %]);
  endif;
enddefine;


example parse_NT
vars expr;
parse_NT([<expr> -> <term> + <expr>]) =>
** expr [-> < term > + < expr >]
endexample

/*
parse_body parses the body of a non-terminal definition.
--------------------------------------------------------
Here NT_1 is the first non-terminal recognised. It is used for
optimisation.
*/

define parse_body(L,W_cond,NT_prev) -> NT_1 -> L1 -> Body;
      lvars L, Body,fail L1 Code;
      parse_tail(L,NT_prev) -> NT_1 -> L1 -> Code;
      if L1 then
          [^W_cond (L -> L1; ^^(Code.tl) then return] -> Code;
          parse_body(L1,"elseif",NT_1) -> NT_1 -> L1 -> Body;
          Code <> Body -> Body;
      else
          [else false -> L1 endif] -> Body;
          L -> L1;
        undef -> NT_1;
      endif
enddefine;

/*
parse_tail parses the tail of a production, starting with the "->"
-------------------------------------------------------------------
*/

define parse_tail(L,NT_prev) -> NT_1 -> L1 -> Code;
    lvars
        L,Code = [],
        L1 = check(L,"->");
    dlocal found_pattern = false, n_NT = 0;
    if L1 then                                                         ;;; Was there an arrow ?
        parse_seq(L1, LVars_t,NT_prev,";") ;;; Y - parse rest of prodn.
            -> NT_1 -> L1 -> Code;

        Code<>
        if found_pattern then []
        else
            if n_NT == 1 then
                [and ( ^(LVars_t.hd)  -> t_out, true)]
            else
                mishap_grm('Clause does not contain pattern or semantic proc call',
                    L);
            endif
        endif -> Code;

    endif                                       ;;; If not, L1 = false

enddefine;

/*
parse_seq parses part of a production following a "->"
---------------------------------------------------------
Parse the sequence following the "->", L_t is list of [t_i..t9], the
variables that hold the actual values of the grammatical classes.
NT_prev is the name of the first non-terminal of the previous
clause, or false.
NT_1 is the name of the first current non-terminal of the
sequence. If a sequence starts with the same non-terminal as the previous
sequence, there is no need to reparse - we just remember the answer.
t1 is the parse, and L0 is the input with prev.
W_termin is the symbol that terminates the sequence.
*/


define parse_seq(L,L_t,NT_prev,W_termin) -> (Code,L1,NT_1);
    lvars
        L,
        L_t,
        L1,W_termin,NT_prev,NT_1,
        Code,                      ;;; POP-11 code to recognise the sequence
        L2 = parse_NT(L),          ;;;| L2 is the list L1 with an NT removed
        NT = identfn(),            ;;;| NT is the POP-11 word (or L2 = false)
        Code1;

    if null(L_t) then
      mishap('Run out of "t" variables - too many non-terminals in clause',[])
    endif;

    lvars
        t1 = L_t.hd,
        L_t1 = L_t.tl;
    if L2 then                       ;;;| Compile code to call a non-terminal
        n_NT + 1 -> n_NT;            ;;; count number of non-terminals met.
        if NT_prev then              ;;; first NT of clause?
            if NT_prev = NT then     ;;; same as 1st NT of previous clause?
                [(L0 ->> L1)]        ;;; yes - used saved list (and t1);
            elseif islist(NT) then
                lvars W = hd(tl(NT));
                [(check(L1," ^W ") ->> L1 ->> L0)]
            else
                [(^NT (L1) ->( ^t1, L1); ;;; Call the NT to parse list L1
                    L1->L0, L1)]    ;;;
            endif;
            NT -> NT_1;
        elseif islist(NT) then
            lvars W = hd(tl(NT));
            [(check(L1," ^W ") ->> L1)]
        else
            [(^NT (L1)-> (^t1,L1), L1)]
        endif -> Code;
        parse_seq(L2,L_t1,false,W_termin)
            -> ;    -> L1 -> Code1;                ;;; Parse the rest of the prodn.
        conjoin_code(Code,Code1) -> Code;

    elseif ( check(L,W_termin)->>L1)then             ;;; End of production.
        [] -> Code;                             ;;; Recognised by null code.

    elseif  isstring(hd(L)) then                ;;; Prolog pattern?
        true -> found_pattern;
        code_of_comm(hd(L)) -> Code;            ;;; Y - get code to generate it
        parse_seq(L.tl,L_t,false,W_termin)
            -> ;   -> L1 -> Code1;              ;;; parse remains of sequence
        conjoin_code(Code,Code1) -> Code;       ;;; combine the code frags.


    else                                        ;;; Terminal symbol
        lvars L3 = dest(L),                     ;;; obj is the symbol.
            obj = identfn();                   ;;; Compile code to check for

        if obj == "->" then
            mishap_grm('"->" not allowed. Is there a missing ";"?',[]);
        endif;

        if obj == "[" and is_extended then
            parse_option(L3,L_t) -> (Code,L3);
            parse_seq(L3,L_t1,false,W_termin)
                -> ; -> L1 -> Code1;                ;;; And carry on parsing the
            conjoin_code(Code,Code1) -> Code;       ;;; rest of the sequence

        elseif obj == "{" and is_extended then
            parse_option_seq(L3,L_t) -> (Code,L3);
            parse_seq(L3,L_t1,false,W_termin)
                -> ; -> L1 -> Code1;                ;;; And carry on parsing the
            conjoin_code(Code,Code1) -> Code;       ;;; rest of the sequence

        else                                       ;;; Generate code to
            ;;; recognise a terminal.
            if isword(obj) then                     ;;; Is the terminal a word?
                [(check(L1," ^obj ") ->> L1)]       ;;; Y- it must be quoted
            else
                [(check(L1, ^obj) ->> L1)]          ;;; N- don't quote it.
            endif -> Code;                          ;;;

            unless member(obj,LW_reserved) then     ;;; Add it to the list of
                obj::LW_reserved -> LW_reserved     ;;; reserved words.
            endunless;                              ;;;

            parse_seq(L3,L_t,false,W_termin)
                -> ; -> L1 -> Code1;                ;;; And carry on parsing the
            conjoin_code(Code,Code1) -> Code;       ;;; rest of the sequence
            endif
        endif

enddefine;

/*
parse_option parses an option enclosed in [...] brackets
---------------------------------------------------------
*/

define parse_option(L,L_t) -> (Code,L_1);
    dlocal found_pattern = false,n_NT = 0;
    lvars (Code,L_1,NT) = parse_seq(L,LVars_t,false,"]");
    lvars t_i = hd(L_t);
    lvars Var_res = Var_result(L);
    [(lblock
        lvars ^^LVars_t, t_out,L_in = L1;
        if ^^Code then ^Var_res else L_in -> L1, false endif
        endlblock -> ^t_i;true)]->Code;
enddefine;



/*
parse_option_seq parses a seqence enclosed in {...} brackets
------------------------------------------------------------
A sequence of terminals/non-terminals can be enclosed in {...} brackets
to indicate repetition. Optionally, the form {....}/SN is used to indicate
that the non-terminals may be separated by separator S and that at least
N repetitions are required.

At (1) we initialise some naughty dlocals which will be side-effected
by  -parse_seq-.
At (2) we call parse_seq to parse the terminal/non-terminal sequence between
the curly brackets.
At (3) we note that the {...} sequence will give rise to abstract syntax
held in the variable "t_1".
At (4) we get the variable which will hold the abstract syntax generated
by the {...}
At (5) we initialise W_sep, the token which separates repetitions, to <false>
indicating the default, that no separating token is expected. We also
initialise the least number of repetitions, n, to 0.
Beginning at (6) we set up W_sep and n if we have the /SN construction.
(6a) Note that if just a number follows the "/", then it is taken to be the
number of repetitions, with no separator.
At (7) we get the code to recognise the separator and count repetitions.
At (8) we get the code to reset the input-token list after the last
repetition.
At (9) we generate the code to recognise the repetition, make a list
of the repeated abstract syntax, and bind it to t_i.

This is not elegant, and uses t_out of the main clause to hold the
boolean result for a while...
*/

define parse_option_seq(L,L_t) -> (Code,L_1);
    dlocal found_pattern = false, n_NT = 0;                   ;;; (1)
    lvars (Code,L_1,NT) = parse_seq(L,LVars_t,false,"}");     ;;; (2)
    lvars t_i = hd(L_t);                                      ;;; (3)
    lvars V_res = Var_result(L);                              ;;; (4)
    lvars W_sep=false,n=0;                                    ;;; (5)
;;;    L_1.tl.tl.tl;
;;;    printf('L_1 = %p\n',[^L_1]);
    if hd(L_1) = "/" then                                     ;;; (6)
        tl(L_1) -> L_1;
        dest(L_1) -> (W_sep,L_1);
        if isnumber(W_sep) then                              ;;; (6a)
           W_sep -> n; false -> W_sep;
        else
        dest(L_1) -> (n,L_1);
        endif;
    endif;
    lvars Code_sep = mk_Code_sep(W_sep,n);                    ;;; (7)
    lvars Code_term = mk_Code_term(n);                        ;;; (8)

    [(lblock lvars n=0;   %"[","%" %
        lvars ^^LVars_t, t_out,L_prev = L1, n=0;
        while ^^Code then ^V_res; L1->L_prev;
        ^^Code_sep;
        endwhile; L_prev -> L1;
        %"%","]",% ;^^Code_term endlblock -> t_out;-> ^t_i; t_out;)]->Code;
enddefine;

/*
Var_result(L) is the name of the variable holding the result of a parse
-----------------------------------------------------------------------
Normally the abstract syntax formed by a parse must be specified as a pattern
(e.g. 'f(^t1,^t2))'. But if there has been only one non-terminal in a clause,
or part of a clause delineated by {....} or [...] brackets, then the abstract
syntax by default is taken to be that generated by the single non-terminal.
*/

define Var_result(L);
    if found_pattern then
        "t_out"
    else
        if n_NT == 1 then hd(LVars_t)
        else
            mishap_grm(
                '[...] option does not contain pattern or semantic proc. call',
                L);
        endif
    endif;
enddefine;


/*
mk_Code_sep makes code to recognise and count separators in a {...} seq.
------------------------------------------------------------------------
*/

define mk_Code_sep(W_sep,n);
    lvars W_sep,n;
    unless isnumber(n) then
       mishap_grm('integer number of repeats reqd for {..}/<t><n>',
                  [^n]);
    endunless;
    if n>0 then [n+1->n;]
    else []
    endif;  <>
    if W_sep then
        [quitunless(check(L1,"^W_sep") ->> L1);]
    else []
    endif
enddefine;

/*
mk_Code_term makes code to tidy up after a {...} sequence
---------------------------------------------------------
*/

define mk_Code_term(n);
  if n>0 then
    [n>= ^n]
  else
  [true]
  endif
enddefine;

/*
conjoin_code makes a POP-11 conjunction out of two code sequences.
-------------------------------------------------------------------
*/


define conjoin_code(Code1,Code2);
   if null(Code2) then Code1
   else Code1 <> [and] <> Code2
   endif
enddefine;

/* Examples - we can't use example because line buffer fills up
parse_seq([<term> + <expr> ; ],[t1 t2 t3 t4 t5],"term",";") =>



parse_body([ -> <id> * <term> ;
             -> id;
             <var> ],"if") =>


parse_class_def([ <expr> -> <id> + <expr> '^t1 + ^t2';
                         -> <id> '^t1';
                 <end>]
                ) =>

code_of_comm generats code to create abstact syntax
---------------------------------------------------

 This procedure generates code to create abstract syntax from the pattern
or to perform a semantic function.
*/


define code_of_comm(String);
  lvars
    rep     = stringin(quote_uc(String)<>' $$\n');
  dlocal
    proglist = rep.incharitem.pdtolist;
  lvars
    Term =  prolog_readterm_to("$$");
  if isword(Term) then
    [(" ^Term " -> t_out, true)]

  else
    lvars f = functor(Term);
    unless is_semantic(f) then
      list_union([^f],L_functors) -> L_functors;
    endunless;
    [ ( ^^( expand(Term))   -> t_out, true) ]
  endif
enddefine;


/*  is this needed????
define macro expand_now(Term);
    expand(Term)
enddefine;
*/

/*
parse_ved_decl treats the VED declaration specifying file extensions.
---------------------------------------------------------------------


The VED declaration is used  to tell the VED  editor about the language  being
defined. VED  <pname> <str1>  <str2>  ... ;  says  that the  procedure  called
<pname> should be used to compile all files having extension <str1> or  <str2>
or ... .
*/

define parse_ved_decl(L) -> L -> x;        ;;;
  lvars L,x=undef,W_comp = L.hd;
  L.tl->L;
  until L.hd = ";" do                       ;;; Get the strings which
    lvars str = L.hd;                       ;;; are the file extensions.
    unless isstring(str) then
      mishap('File extension must be string',[^str]);
    endunless;
    [[^str {popcompiler ^W_comp}]          ;;; Associate extension with
     [^str {vedcompileable true}]          ;;; W_comp as compiler, and
    ]                                      ;;; specify such files as being
      <> vedfiletypes                      ;;; compilable.
    -> vedfiletypes;

    compile_pr([

    [[^str {popcompiler ^W_comp}]          ;;; Associate extension with
     [^str {vedcompileable true}]          ;;; W_comp as compiler, and
    ]                                      ;;; specify such files as being
      <> vedfiletypes                      ;;; compilable.
    -> vedfiletypes;
    ],Rep_code);
    L.tl->L;
  enduntil;
  L.tl->L;
enddefine;

/*
parse_pantex_decl allow typographic presentation with PANTECHNICON
------------------------------------------------------------------
*/


define parse_pantex_decl(L) -> (x,L_out);
  lvars L,x,L_out,x,
    (Compiler,L1) = dest(L);
  if Compiler = ";" then return(L1->L_out);
  endif;
  lvars  (Parser,L2) = dest(L1),
         (Style,L3)  = dest(L2);
  compile_pr([uses Pantechnicon;],Rep_code);
  compile_pr(["^Parser" -> Pantechnicon$-Parser_of_Compiler("^Compiler");],
             Rep_code);
  compile_pr(["^Style" -> Pantechnicon$-Style_of_Compiler("^Compiler");],
             Rep_code);
  parse_pantex_decl(L3) -> (x,L_out);

enddefine;




/*
parse_output_decl treats an OUTPUT declaration - where is code to go?
---------------------------------------------------------------------

An output declaration specifies where the POP-11 code for the grammar is to be
written.
*/

define parse_output_decl(L) -> L -> x;        ;;;
  lvars L,x,FN = hd(L);
    unless isstring(FN) then
      mishap('File name in OUTPUT declaration must be string',[^FN]);
    endunless;
    L.tl->L;
    unless hd(L) = ";" then
      mishap('OUTPUT declaration must end with semicolon');
    endunless;
    L.tl -> L;                                ;;; Skip the semicolon.
  FN -> FN_out;

enddefine;

/*
Grammar files have the extension '.grm'
-----------------------------------------
The statements below tell VED how to handle grammar files.
*/

[['.grm' {popcompiler parse_gen_compile}]
 ['.grm' {vedcompileable true}]
]
    <> vedfiletypes
-> vedfiletypes;




;;; The old versions are saved for non-parse_gen compilers:
;;; We use parse_first_time to assist in debugging parse_gen - we would
;;; not want to keep re-assigning to old_mbp etc.

if isundef(parse_first_time) then
   ved_mbp -> old_mbp;
   ved_mep -> old_mep;
endif;

false -> parse_first_time;


define ved_mbp();
    lvars    start, line;
    dlocal    vedline, vedcolumn, vvedlinesize;
    unless popcompiler == compile_parse_gen_no_obj do
        chain(old_mbp)
    endunless;
    vedline -> start;
    until vvedlinesize fi_> 0
    and fast_subscrs(1, vedthisline() ->> line) = `<`
    do
        if vedline == 1 then
            vederror('No declaration keyword before line ' sys_>< start);
        endif;
        vedcharup();
    enduntil;
    vedmarklo();
enddefine;

define ved_mep();
    lvars    start, line;
    dlocal    vedline, vedcolumn, vvedlinesize;
    unless popcompiler == compile_parse_gen_no_obj do
        chain(old_mep)
    endunless;
    vedline -> start;
    until vvedlinesize fi_> 0
    and fast_subscrs(1, vedthisline() ->> line) = `<`
    do
        if vedline == vvedbuffersize then
               vedmarkhi(); return;
        endif;
        vedchardown();
    enduntil;
    vedcharup();
    vedmarkhi();
enddefine;

/*
mishap_grm treats errors in grammar definitions.
-----------------------------------------------
*/

define mishap_grm(Msg,L_culprits);
    pr('\nError in grammar definition -\n');
    pr(Msg);
    unless null(L_culprits) then
        pr('\nThe culprit was: ');
        pr(L_culprits);
    endunless;
    pr('\nDefining non-terminal "');
    pr(NT_current); pr('"\n');
    setpop();
enddefine;

/*
The macro <$ computes list of tokens
------------------------------------
It is useful because, unlike the [..] construction, no sub-lists are formed.
*/

define macro <$ ;
    lvars tok;
    [%until (readitem() ->> tok) = "$>" do
      if isnumber(tok) and tok < 0 then
        "-", -tok
      else
        tok
      endif;
    enduntil%]
enddefine;

;;; ???
endsection;

uses parse_gen_RT;

;;; ???
compile(stringin(PARSE_GEN$-Str_prologue));
