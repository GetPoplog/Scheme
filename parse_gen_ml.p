
;;; parse_gen.p                     R.J.Popplestone sep90-sep91



compile_mode:pop11 +defpdr +varsch +constr;

uses lexical_args;
uses memo_T_2_n;
uses int_parameters;
uses parse_gen_RT;

;;;define memo_n(f,m,n);
;;;  memo_T_2_n(f,n);
;;;enddefine;



section PARSE_GEN => compile_parse_gen
                     compile_parse_gen_no_obj
                     call_parse_gen  <$;

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

uses dollar_dollar;
uses ved_source;

vars procedure(
     mk_term     = prolog_maketerm,      ;;; Makes a term
     arity        = prolog_arity,         ;;;
     arg         = prolog_arg,           ;;;
     functor     = prolog_functor,       ;;;
     is_complex  = prolog_complexterm,   ;;;
     is_atom     = isword,               ;;; The POP-11 "word" datatype is
     pr_term     = prolog_write,
     check_as_class,
     code_of_comm,
     compile_parse_gen,
     conjoin_code,
     is_semantic = newassoc([]),
     mishap_gr,
     parse_class_defs,
     parse_class_def,
     parse_NT,
     compile_parse_gen_from_rep,
     parse_operator_declaration,
     parse_pop_11,
     parse_seq,
     parse_tail,
     parse_ved_decl,
     parse_body,
     record_HWM,
     check,
     require,
     expand
     old_mbp,
     old_mep,
         );

vars
    FN_out             = false,
    found_pattern      = false,
    parse_first_time,
    L_NT,                  ;;; The non-terminal symbols which have been defined
    L_semantic = [],
    L_undef = [],
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
;





;;; Generate code to make a term in the abstract syntax.

define expand(r);                                ;;; Make term on stack
    lvars r;
    [%
      if prolog_complexterm(r) then
          lvars i,
             f = prolog_predword(r),
             n = prolog_nargs(r);
         if    f = "^" and n=1 then               ;;;  ^(f(a1..an)) OR ^(x)
            lvars r1 = arg(1,r),
                  n1 = prolog_nargs(r1);
            if isword(r1) then                   ;;; we had ^(x), now r1 = x
                "(",
                 r1,                             ;;; push its value on stak
            elseif prolog_complexterm(r1) then   ;;; we had ^(f(x,y))
              "mk_term","(",
                 for i from 1 to n1 do
                   explode(
                   expand(arg(i,r1))     ;;; make the i'th  argument
                   ); ",";
                 endfor;
                    functor(r1);           ;;; push the value of f
                    ",",
                    n1                           ;;; push the number of args.
            else                                 ;;; Cannot take value of
                   mishap('Must have identifier ',  ;;; constant
                      [^r])                      ;;;
            endif
        else                                     ;;; ordinary complex term
       "mk_term","(",
            for i from 1 to n do                 ;;; f(a1...an)
              explode(
              expand(arg(i,r))            ;;; make i'th argument
              ); ",";
            endfor;
            """,
            functor(r),                    ;;; push "f"
            """,",",
            n                                    ;;; push the number of args.
        endif;
      ")"
    elseif isword(r) then                        ;;; A word.
           """,r,""",
    else                                         ;;; Not a complex term
           r
      endif

    %]
enddefine;

example expand
vars T;
prolog_readterm();  f(a,1,^b). -> T;
expand(T) =>
** [mk_term ( " a " , 1 , ( b ) , " f " , 3 )]
expand($$a+c*d$$)=>
** [mk_term ( " a " , mk_term ( " c " , " d " , " * " , 2 ) , " + " , 2 )]

endexample

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
  lambda                     = PARSE_GEN$-lambda,
  null_G                     = PARSE_GEN$-null_G,
  check                      = PARSE_GEN$-check,
  Parse_expr_p               = PARSE_GEN$-Parse_expr_p,

);
';

->pop_longstrings;

/* This procedure compiles a grammar definition. It takes the same
arguments as the POP-11 standard procedure compile except for a device.
*/

define compile_parse_gen(Source);
  lvars Source, Rep;
  dlocal                             ;;; Initialise ....
    pop_default_type = '.grm',       ;;; The default file extension.
    FN_out      = false,             ;;; The file name for the output file.
    L_NT        = [],                ;;; The list of defined non-terminals
    LW_reserved = [-> ; <            ;;; Reserved words.
                   ^(consword('`'))
                   ^(consword('\''))
                   ],                ;;; The list of reserved words.
    L_section  = false,              ;;; Is a section specified in the grammar?
      ;

  if isprocedure(Source) then        ;;; Get the appropriate repeater for
    Source                           ;;; source.
  elseif isword(Source)
  or     isstring(Source) then
    discin(Source)
  elseif isdevice(Source) then
    mishap('cant compile device',[^Source]);
  else
    mishap('Cant compile grammar',[^Source]);
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
              else Source<>'.p'
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

  pr(Str_prologue);

  compile_pr_list(LW_reserved,cucharout);      ;;; Assign to  list of reserved words
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
                 datalength(FN_out)-2,    ;;; which will ensure the
                 FN_out));                ;;; file is only loaded once
   pr('\nvars ' >< Wd >< '= true;\n');

  Rep_obj(termin);                   ;;; Close the object file.
enddefine;

define compile_parse_gen_no_obj(Rep);
  dlocal Rep_code = erase,
         L_NT = [];
  compile_parse_gen_from_rep(Rep);
enddefine;

vars procedure parse_gen = compile_parse_gen;  ;;; For compatibility;

define compile_parse_gen_from_rep(Rep);
  lvars
       L =  Rep.incharitem.pdtolist,
       W,
;
  dlocal L_undef = [],               ;;; Naughty dlocal.. - holds undefined NTs
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

define parse_class_defs(L);
  while (parse_class_def(L)->>L) do
  endwhile
enddefine;

;;; This procedure parses the definition of a non-terminal symbol
;;; or other statement in parse_gen.

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

    if Tok = "SEMANTIC" then          ;;; List of names of semantic procs.
         L.tl->L;
        until hd(L) = ";" do
            dest(L) -> L -> f;
              true -> is_semantic(f);
        enduntil;
         return(L.tl->L2);
    endif;

    if Tok = "OPERATOR" then
       parse_operator_declaration(L.tl) -> L2;
       return;
    endif;

    if Tok = "VED" then
      parse_ved_decl(L.tl) -> L2 -> ;
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
        mishap_gr('Non-terminal definition must start with a non-terminal name',
               [^Tok]);
      endunless;

      NT -> NT_current;              ;;; Make this the current non-terminal
      lvars
         junk  = parse_body(L1,"if",true),   ;;; parse the body of the definition
        L2    = identfn(),
         body  = identfn(),             ;;; obtaining (body,L2)
         code  = [define ^NT(L) -> (t_out,L1);
        lvars L,L0,L1,t_out=undef,
        t1, t2, t3, t4, t5,
        t6, t7, t8, t9;
        ^^body
        enddefine;];

  if mon_parse_gen then
    pr(code); nl(1);
  endif;

  compile_pr(code,Rep_code);          ;;; Compile the code for the NT
  record_definition(NT);
enddefine;


define record_definition(NT);
  delete(NT,L_undef) -> L_undef;              ;;; NT is now defined...
  NT::L_NT -> L_NT;
enddefine;

;;; Parse a declaration for a NT class to be treated as infix operators.

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
     enddefine;
     ml_val ^NT : TokList -> TokList*ASTree = ^NT;
     ];

  if mon_parse_gen then
    pr(code); nl(1);
  endif;

  compile_pr(code,Rep_code);
  record_definition(NT);
  tl(L7) -> L_out;
enddefine;

;;; Parse a section of POP_11 code terminated by a semicolon
;;; at bracket nesting depth 0.

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
        mishap_gr('Non-terminal name must be POP-11 word',[^W_class]);
    endunless;
    unless p=undef or p=0 then
        mishap_gr('Non-terminal name must NOT be POP-11 reserved word',[^W_class]);
    endunless;
    if isundef(valof(W_class))
    and not(member(W_class, L_undef))
    then
      W_class::L_undef -> L_undef
    endif;
        require(L.tl,">") -> L;
enddefine;

define require(L,tok) -> L;
  lvars L,tok;
  if    L.hd = tok then L.tl -> L
  else  mishap_gr('Missing token',[% L.hd, tok %]);
  endif;
enddefine;


example parse_NT
vars expr;
parse_NT([<expr> -> <term> + <expr>]) =>
** expr [-> < term > + < expr >]
endexample

;;; parse the body of a non-terminal definition.

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

;;; Parse the tail of a production, starting with the "->"

define parse_tail(L,NT_prev) -> NT_1 -> L1 -> Code;
    lvars
             L,Code = [],
             L1 = check(L,"->");
    dlocal found_pattern = false;
    if L1 then                                                         ;;; Was there an arrow ?
      parse_seq(L1, [t1 t2 t3 t4 t5 t6 t7 t8 t9],NT_prev) ;;; Y - parse rest of prodn.
            -> NT_1 -> L1 -> Code;
    unless found_pattern then
       pr('\n Clause does not contain pattern or semantic proc call\n'><
           L><'\n');
    endunless
  endif                                       ;;; If not, L1 = false
enddefine;

;;; Parse the sequence following the "->", L_t is list of [t_i..t9], the
;;; variables that hold the actual values of the grammatical classes.
;;; NT_prev is the name of the first non-terminal of the previous
;;; clause, or false.
;;; NT_1 is the name of the first current non-terminal of the
;;; sequence,

define parse_seq(L,L_t,NT_prev) -> NT_1 -> L1 -> Code;
      lvars
         L,
         L_t,
         L1,
         Code,                      ;;; POP-11 code to recognise the sequence
         L2 = parse_NT(L),          ;;;| L2 is the list L1 with an NT removed
         NT = identfn(),            ;;;| NT is the POP-11 word (or L2 = false)
            Code1,
            t1 = L_t.hd,
            L_t1 = L_t.tl;
      if L2 then                                   ;;;| Compile code to call a
                                               ;;;| non-terminal proc.
        if NT_prev then                        ;;; first NT of clause?
          if NT_prev = NT then       ;;; same as 1st NT of previous clause?
            [(L0 ->> L1)]            ;;; yes - used saved list (and t1);
          else
            [(^NT (L1) ->( ^t1, L1); ;;; Call the NT to parse list L1
                     L1->L0, L1)]    ;;;
          endif;
          NT -> NT_1;
        else
           [(^NT (L1)-> (^t1,L1), L1)]
        endif -> Code;
           parse_seq(L2,L_t1,false)
         -> ;    -> L1 -> Code1;                ;;; Parse the rest of the prodn.
        conjoin_code(Code,Code1) -> Code;

      elseif ( check(L,";")->>L1)then             ;;; End of production.
        [] -> Code;                             ;;; Recognised by null code.

      elseif  isstring(hd(L)) then                ;;; Prolog pattern?
        true -> found_pattern;
        code_of_comm(hd(L)) -> Code;            ;;; Y - get code to generate it
        parse_seq(L.tl,L_t,false)
                      -> ;   -> L1 -> Code1;    ;;; parse remains of sequence
        conjoin_code(Code,Code1) -> Code;       ;;; combine the code frags.

      else                                        ;;; Terminal symbol
        lvars L3 = dest(L),                     ;;; obj is the symbol.
             obj = identfn();                   ;;; Compile code to check for

        if obj = "->" then
              mishap_gr('"->" not allowed. Is there a missing ";"?',[]);
        endif;
                                                ;;; Generate code to
                                                ;;; recognise a terminal.
        if isword(obj) then                     ;;; Is the terminal a word?
            [(check(L1," ^obj ") ->> L1)]       ;;; Y- it must be quoted
        else
            [(check(L1, ^obj) ->> L1)]          ;;; N- don't quote it.
         endif -> Code;                          ;;;

        unless member(obj,LW_reserved) then     ;;; Add it to the list of
            obj::LW_reserved -> LW_reserved     ;;; reserved words.
        endunless;                              ;;;

        parse_seq(L3,L_t,false)
                     -> ; -> L1 -> Code1;       ;;; And carry on parsing the
        conjoin_code(Code,Code1) -> Code;       ;;; rest of the sequence

      endif
enddefine;

define conjoin_code(Code1,Code2);
   if null(Code2) then Code1
   else Code1 <> [and] <> Code2
   endif
enddefine;

/* Examples - we can't use example because line buffer fills up
parse_seq([<term> + <expr> ; ],[t1 t2 t3 t4 t5],"term") =>



parse_body([ -> <id> * <term> ;
             -> id;
             <var> ],"if") =>


parse_class_def([ <expr> -> <id> + <expr> '^t1 + ^t2';
                         -> <id> '^t1';
                 <end>]
                ) =>

*/

;;; This procedure generates code to create abstract syntax from the pattern
;;; or to perform a semantic function.

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
        lvars f = functor(Term),
                 n = prolog_nargs(Term),
         i;

        if is_semantic(f) then
        [% "(", f, "(",
         for i from 1 to n do
           explode(expand(arg(i,Term))), ",",
         endfor,
         ")","->", "t_out",";", "true",")" %]
        else
      lvars f  = functor(Term),
            f1 = if f = "^"
                 and prolog_nargs(Term) = 1
                 then arg(1,Term)
                 else f
                 endif;

      list_union([^f1],L_functors) -> L_functors;
           [ ( ^^( expand(Term))   -> t_out, true) ]
        endif;
  endif
enddefine;

/*

define arg_gr(i,Term) -> T_i;
  lvars i,Term, T_i = arg(i,Term);
  if prolog_complexterm(T_i)
  and functor(T_i) = "^"
  and prolog_nargs(T_i) = 1
  then arg(1,T_i) -> T_i
  endif;
enddefine;
*/

define macro expand_now(Term);
    expand(Term)
enddefine;

;;; The VED declaration is used to tell the VED editor about the
;;; language being defined. VED <pname> <str1> <str2> ... ; says that
;;; the procedure called <pname> should be used to compile all
;;; files having extension <str1> or <str2> or ... .

define parse_ved_decl(L) -> L -> x;        ;;;
  lvars L,L,x=undef,W_comp = L.hd;
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

;;; An output declaration specifies where the POP-11 code for the
;;; grammar is to be written.

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

;;; The statements below tell VED how to handle grammar files.
;;; and should not be confused with

[['.grm' {popcompiler compile_parse_gen_no_obj}]
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

define mishap_gr(Msg,L_culprits);
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

endsection;

uses parse_gen_RT;
